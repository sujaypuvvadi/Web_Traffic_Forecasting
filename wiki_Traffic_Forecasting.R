
##################################################################
# Forecasting the traffic for Wikipedia Web Page
# Data Time Frame :- July, 1st, 2015 up until December 31st, 2016
##################################################################

# Clear all the variables in workspace
rm(list = ls())

# Load the packages
library(fpp2)

require(data.table)
require(TSA)
require(forecast)
require(xts)
require(tseries)
require(graphics)
#require(dplyr)
#require(ggplot2)
require(tidyverse)
library(stringr)
library(plotly)
library(parallel)
pacman::p_load(wordcloud,corpus) # textmining


# Setting the working drectory
setwd('D:\\github\\dataSets\\kaggle\\web_traffic_forecasting\\')

# Load the data
train <- fread("train_1.csv")


#####################################
# Data Exploration and Visulizations
#####################################

############## Sampling the Data #########################

# Here I sampled the train data to 1/10 of all wiki Pages for faster runtime, 
# And will later iterate through each sample for the analysis. 
# This time we will analyze only the language specific projects and but drop the ones that belong to wiki media.

set.seed(1234)

sample_wiki <- train %>% # we use the full sample now
  filter(!(grepl('www.mediawiki.org',Page))) %>%
  sample_frac(0.1) %>%
  gather(Date, Visit, -Page) %>% data.table

################ Accessing the hidden data (Feature Enginering) #################

# extract name, project, access, agent from Page
name = mclapply(str_split(sample_wiki$Page,pattern = "_"),
                function(x) head(x,length(x)-3))
name = mclapply(name, function(x) paste(x,collapse = ' '))

page_split <- mclapply(str_split(sample_wiki$Page,pattern = "_"), 
                       function(x) tail(x,3)) 
add <- data.table(Project= unlist(mclapply(page_split, function(x) x[1])),
                  Access= unlist(mclapply(page_split, function(x) x[2])),
                  Agent= unlist(mclapply(page_split, function(x) x[3])),
                  Name = unlist(name))

sample_wiki <- cbind(sample_wiki, add)
head(sample_wiki)[,-1]

######################### Missing Values by Groups ##########################

# Count percentage of missing values by Project
table(sample_wiki[is.na(Visit), Project])/table(sample_wiki[, Project])

# let's treat the missing values from Visits as 0. 
# We are unsure if they should be treated as zero visits to a Page on a specific day or maybe something happend on the server end and failed to update numbers. 
# Either way we need to somehow imply that similar patterns can be applied in the future.

sample_wiki <- replace_na(sample_wiki,list(Visit = 0))

############################ Feature ENgineering ##########################

sample_wiki <- sample_wiki %>% 
  mutate(Date = as.Date(Date,format="%Y-%m-%d"),
         Year = year(Date),
         Month = month(Date),
         Visit_m = Visit/1000000)

########################## Exploration ########################

# calculate average monthly visits
p_month <- sample_wiki %>%
  mutate(year_month = format(Date, "%Y-%m")) %>%
  group_by(year_month, Project) %>%
  summarise(Visit = mean(Visit)) %>%
  ggplot(aes(year_month, Visit)) + 
  geom_bar(stat = 'identity', aes(fill = Project)) + 
  theme_classic(base_size = 12,base_family = 'mono') + 
  ylab('Number of Visits') + xlab('Year - Month') + ggtitle('Average Monthly Wikipedia Traffic')
ggplotly(p_month)


# Visualize the sample data, by Project
p_proj <- sample_wiki %>%
  group_by(Date,Project) %>%
  summarise(Visit_m = sum(Visit_m)) %>%
  ggplot(aes(Date, Visit_m)) + 
  geom_line(aes(color = Project), size = 0.3) + 
  # facet_grid(~Project,scales = 'free_y',shrink = F) + 
  theme_classic(base_size = 12,base_family = 'mono') +
  theme(legend.position = 'top') +
  ggtitle('Traffic in Wikipedia by Projects') +
  ylab('Visit in Millions')
ggplotly(p_proj)

# Visualize by Access
p_access <- sample_wiki %>%
  group_by(Date,Access) %>%
  summarise(Visit_m = sum(Visit_m)) %>%
  ggplot(aes(Date, Visit_m)) + 
  geom_line(aes(color = Access)) + 
  theme_classic(base_size = 12,base_family = 'mono') + 
  ggtitle('Taffic by Access') +
  ylab('Visit in Millions')
ggplotly(p_access)


# summarize by Project, pick the top 1 of all time
top_1_proj <- sample_wiki %>%
  group_by(Project, Name) %>%
  summarise(Visit = sum(Visit)) %>%
  top_n(1, Visit) %>% data.table
top_1_proj

#From the summary above, I see that pages with keyword Special: is the most visited from the English wiki project, 
# And have reached more than 1 billion in 500 days. 
# This is worth noticing and could be a potential outlier.

# summarize by project and year, top 1
top_1_proj_yr <- sample_wiki %>%
  group_by(Project, Year, Name) %>%
  summarise(Visit = sum(Visit)) %>%
  top_n(1, Visit) %>%
  spread(Year,Visit) %>% data.table
top_1_proj_yr

################# Word CLoud ###########################

wc <- sample_wiki %>% 
  group_by(Project, Year, Name) %>%
  summarise(Visit = sum(Visit)) %>% data.table

wc_en <- wc[grepl('en',Project) & !grepl(Name,pattern = c('Special:'))]
wc_en_15 <- wc_en[Year == 2015]
wc_en_16 <- wc_en[Year == 2016]

# 2015
set.seed(1234)
wordcloud(words = wc_en_15$Name, freq = wc_en_15$Visit, min.freq = 1,
          max.words=round(nrow(wc_en_15)*0.1,0), random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")
          # random.color = F
)

# 2016
set.seed(1234)
wordcloud(words = wc_en_16$Name, freq = wc_en_16$Visit, min.freq = 1,
          max.words = round(nrow(wc_en_16)*0.1,0), random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2")
          # random.color = F
)

# 2015
top_10_en_15 <- top_n(wc_en_15, 10,Visit) %>% select(Name)
# time trend by the top phrases
sample_wiki %>% 
  filter(Name %in% top_10_en_15$Name,
         Year == 2015) %>%
  ggplot() + 
  geom_bar(aes(x= Date,y = Visit_m), stat = 'identity', fill = 'red',alpha = 0.7) +
  facet_wrap(~Name, scales = 'fixed',nrow = 5) +
  theme_classic(base_size = 12,base_family = 'mono') + ylab('Visit in Millions') +
  ggtitle('Top 10 Visited Pages in 2015')

# 2016
top_10_en_16 <- top_n(wc_en_16, 10,Visit) %>% select(Name)
# time trend by the top phrases
sample_wiki %>% 
  filter(Name %in% top_10_en_16$Name,
         Year == 2016) %>%
  ggplot() + 
  geom_bar(aes(x= Date,y = Visit_m), fill = 'red', alpha = 0.7, stat = 'identity') +
  facet_wrap(~Name, scales = 'free_y', nrow = 5) +
  theme_classic(base_size = 12,base_family = 'mono') + ylab('Visit in Millions') +
  ggtitle('Top 10 Visited Pages in 2016')

#######################################
# Data Pre-Processing and Explorations
#######################################

head(train)

# From the data we can see there are lot of web Pages in the website.
# so lets'start picking an web page with maximum number of visit to analyze and build the model.

train$sum = rowSums(train[,2:551])

head(train %>% select(Page,sum) %>% arrange(desc(sum)))

top.Pages <- train %>% 
  select(Page,sum) %>% 
  arrange(desc(sum)) %>%
  #mutate(sum=log(sum)) %>%
  head() 

ggplot(data = top.Pages) + geom_bar(mapping = aes(x = Page,y=sum),stat = 'identity',fill='coral1') +
  xlab("Web Pages") +
  geom_label(aes(Page, sum, label = sum), vjust = 0.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Overall Page Visits")

# We can see that Main Page is the one that getting maximum views/traffic.
# Let's Start modelling on that and make it generalize to every page

trainsep = train[which(train$sum == max(na.omit(train$sum))),]

f = t(trainsep[,-c(1,552)])
f = data.frame(f,row.names(f))
colnames(f) = c("f","d")
f$d = as.POSIXct(f$d, format="%Y-%m-%d")

t2_xts = xts(f$f, order.by = f$d)
t2_xts = t2_xts/1000


################################
# Splitting into train and test
################################

####################### Training set from 2015-07-01 to 2016-11-30 ############################

start_date = as.POSIXct("2015-07-01", format="%Y-%m-%d")
end_date = as.POSIXct("2016-11-30", format="%Y-%m-%d")
t2_tr = window(t2_xts, start = start_date, end = end_date)

#Converting the xts class to data frame format
t2_tr = data.frame(index(t2_tr),t2_tr)

#Changing the column names of t2_tr
colnames(t2_tr) = c("d","f")
rownames(t2_tr) = rownames("")

######################### Test set from 2016-12-01 till 2016-12-31 ############### 
start_date = as.POSIXct("2016-12-01", format="%Y-%m-%d")
end_date = as.POSIXct("2016-12-31", format="%Y-%m-%d")
t2_te = window(t2_xts, start = start_date, end = end_date)

t2_te = data.frame(index(t2_te),t2_te)

#Changing the column names of t2_te
colnames(t2_te) = c("d","f")
rownames(t2_te) = rownames("")


########################################
# Preliminary Analysis Before Modelling
########################################

########### Time Series PLot ########################
autoplot(t2_xts) + 
  ggtitle('Time Plot: Page Views Per Day') +
  ylab('Views (K)')

# Data doesnt has strong trend, this is due to less data(less than 2 years).
# But I can find seasonality, and outliers (peaks)


################### Test for stationarity ################

library(tseries)
adf.test(t2_tr$f) # p-valuse < 0.05

# test shows that our data is stationarity.

################## Investigating the seasonality ###############

# The data is from 2015-07-01 to 2016-12-31. there for it is less than two years and totally of 15 months of data. 
# we can apply weekly seasonality and monthly seasonality.  
# if the data is more than two years, we can also apply yealy seasonality.

# Theoretically there are only two seasonality 
y_2 <- msts(t2_tr$f, seasonal.periods=c(7,4.34*7))


autoplot(stl(y_2,s.window="periodic",robust=TRUE))
#plot clearly showed the seasonal and trend part.


##########################
# Basic Approach 
# Simple Model with the average of activity by weekdays
#########################

avg.wday <- t2_tr %>%
            mutate(week_day = wday(d)) %>%
            column_to_rownames(.,var='d') %>%
            group_by(week_day) %>%
            summarise(views = median(f))

pred.values <- t2_te %>%
              mutate(week_day = wday(d))

model.values <- merge(avg.wday,pred.values,by='week_day',all=T) %>% arrange(d)

# Absolute Error
simplemodel.abs.error = abs(sum(model.values$f)-sum(model.values$views))/sum(model.values$f)

# comparing Errors
ggplot(data=model.values) +
  geom_line(mapping = aes(x=d,y=f),color='blue') +
  geom_line(mapping = aes(x=d,y=views),color='red') +
  ggtitle('Simple Averaing weekdays Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

# 0.19102 - abs error

##############################33
# Linear Regression
###############################
model <- lm(f~d,t2_tr)

summary(model)

pred.values <- predict(model,t2_te)

linear.abs.error = abs(sum(t2_te$f)-sum(pred.values,na.rm = T))/sum(t2_te$f)
# 0.180

df <- data.frame(actual = t2_te$f,predicted = pred.values,date=t2_te$d)

ggplot(data=df) +
  geom_line(mapping = aes(x=date,y=actual),color='blue') +
  geom_line(mapping = aes(x=date,y=predicted),color='red') +
  ggtitle('Linear Regression Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

######################################
# Forecasting Modelling - 4 tpyes
# TBATS, BATS, ARIMA, STLM
#####################################

#####################
# 1.) TBATS 
# TBATS is an exponential smoothing model with Box-Cox transformation, ARMA errors, trend and seasonal components. 
# It tunes its parameters automatically. Very cool model, but it can't use external regressors
#####################

# fitting/training
tbats.model = tbats(y_2)
checkresiduals(tbats.model)

# forecasting the values
tbats.forecast = forecast(tbats.model,h=31)

# Visulization
autoplot(tbats.forecast) + 
  ggtitle('Forecasting the wikipedia Page for the next month(TBATS)') +
  ylab('Views (K)') +
  xlab("Days")
  

#Lets check on accuracy part 
accuracy(tbats.forecast$mean,t2_te$f) 
  
#              ME    RMSE     MAE      MPE     MAPE
#Test set 2997230 3423985 3045547 12.13825 12.39839

###################### Absolute Error ################################

#In real life absolute error rate may add more value to business.
tbatforecast = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean)
colnames(tbatforecast) = c("d","actuals","tbats.forecast")

tbats.abs.error = abs(sum(tbatforecast$actuals)-sum(tbatforecast$tbats.forecast))/sum(tbatforecast$actuals)
tbats.abs.error

#0.1271615 - TBATS have performed approximatly 12% of error. 

############################# Prediction vs Actual Values ####################

#Lets have a look on the error
autoplot(cbind(tbatforecast$actuals, tbatforecast$tbats.forecast)) +
  ggtitle('TBATS Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

## From the graph we can see that the model was able to capture the seasonality but not the peaks. 
## Lets explore and confirm on residuals

tbatforecast$Residual = abs(tbatforecast$actuals - tbatforecast$tbats.forecast)

autoplot.zoo(tbatforecast$Residual) +
  ylab('Residuals')
## by residual plot we confirm that the model is not able to predict the peak values.
## Lets have the TBATS as the bench mark.

##########################
# 2.) BATS 
#########################

##################### fitting and forecasting #######################
bats.model = bats(y_2)
checkresiduals(bats.model)

# forecasting the values
bats.forecast = forecast(bats.model,h=31)

# Visulization
autoplot(bats.forecast) + 
  ggtitle('Forecasting the wikipedia Page for the next month(BATS)') +
  ylab('Views (K)') +
  xlab("Days")

#Lets check on accuracy part 
accuracy(bats.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 3738600 4147897 3748747 15.2777 15.33233

###################### Absolute Error ################################

#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,bats.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","bats.forecast")

bats.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$bats.forecast))/sum(forecastValue$actuals)
bats.abs.error

#0.1586151
#BATS have perform badly when compared to TBATS.

############################# Prediction vs Actual Values ####################

#Lets have a look on the error 

autoplot(cbind(forecastValue$actuals, forecastValue$bats.forecast)) +
  ggtitle('BATS Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

batsResidual = abs(forecastValue$actuals - forecastValue$bats.forecast)

autoplot.zoo(tbatforecast$Residual) +
  ylab('Residuals')

#########################
# 3.) STLM with ARIMA 
#########################

####################### fitting and forecasting ##############

stlm.model = stlm(y_2,s.window="periodic")
checkresiduals(stlm.model)

# forecasting the values
stlm.forecast = forecast(stlm.model,h=31)

# Visulization
autoplot(stlm.forecast) + 
  ggtitle('Forecasting the wikipedia Page for the next month(STLM)') +
  ylab('Views (K)') +
  xlab("Days")


#Lets check on accuracy part 
accuracy(stlm.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 4115535 4577968 4122950 16.7827 16.82066

#The model is really performing poor.

###################### Absolute Error ################################

#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,stlm.forecast$mean,bats.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","stlm.forecast","bats.forecast")

stlm.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$stlm.forecast,na.rm = T))/sum(forecastValue$actuals)
stlm.abs.error

#0.1746071
#STLM have perform badly when compared to Tbats. 

############################# Prediction vs Actual Values ####################

autoplot(cbind(forecastValue$actuals, forecastValue$stlm.forecast)) +
  ggtitle('STLM Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

stlmResidual = abs(forecastValue$actuals - forecastValue$stlm.forecast)

autoplot.zoo(stlmResidual)+
  ylab('Residuals')

#####################
# 4.) ARIMA  
# The goal of this notebook is to show how to tune ARIMA model with additional regressors. 
# We will add some Fourier terms to capture multiple seasonality and compare the best model with TBATS model.
#####################

####################### Fitting and forecasting ####################3333

bestfit = list()
bestfit <- list(aicc=Inf)

for(i in 1:3) {
  for (j in 1:3){
    f1xreg <- fourier(ts(t2_tr$f, frequency=7), K=i)
    f2xreg <- fourier(ts(t2_tr$f, frequency=7*4.34), K=j)
    arima.model <- auto.arima(t2_tr$f, xreg=cbind(f1xreg, f2xreg), seasonal=F)
    if(arima.model$aicc < bestfit$aicc) {
      bestfit <- list(aicc=arima.model$aicc, i=i, j=j, fit=arima.model)
    }
  }
}

xregm=cbind(fourier(ts(t2_tr$f, frequency=7), K=bestfit$i, h=31),
            fourier(ts(t2_tr$f, frequency=7*4.34), K=bestfit$j, h=31))

checkresiduals(bestfit$fit)

# forecasting the values
arima.forecast <- forecast(bestfit$fit, xreg=xregm)

# Visulization
autoplot(tbats.forecast) + 
  ggtitle('Forecasting the wikipedia Page for the next month(TBATS)') +
  ylab('Views (K)') +
  xlab("Days")

#Lets check on accuracy part 
accuracy(arima.forecast$mean,t2_te$f)

#              ME    RMSE     MAE     MPE     MAPE
#Test set 4439896 4804145 4439896 18.27674 18.27674

#The model is really performing poor.

###################### Absolute Error ################################

#in real life absolute error rate may add more value to business.
forecastValue = data.frame(t2_te$d,t2_te$f,tbats.forecast$mean,stlm.forecast$mean,bats.forecast$mean,arima.forecast$mean)
colnames(forecastValue) = c("d","actuals","tbats.forecast","stlm.forecast","bats.forecast","arima.forecast")

arima.abs.error = abs(sum(forecastValue$actuals)-sum(forecastValue$arima.forecast))/sum(forecastValue$actuals)
arima.abs.error

#0.1883685
#ARIMA have perform badly whem compared to Tbats. 

############################# Prediction vs Actual Values ####################

#Lets have a look on the error 
autoplot(cbind(forecastValue$actuals, forecastValue$arima.forecast)) +
  ggtitle('ARIMA Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))

## from the graph we can see that the model does not able to capture anything. 
## Lets explore and confirm on residuals

arimaResidual = abs(forecastValue$actuals - forecastValue$arima.forecast)
autoplot.zoo(arimaResidual)

########################
# Ensemble
#######################

# Averaging the mdels
forecasted.values <- 0.8* tbats.forecast$mean + 0.2 * bats.forecast$mean

# Predicting the test data
forecastValue.ensemble = data.frame(t2_te$d,t2_te$f,forecasted.values)
colnames(forecastValue.ensemble) = c("d","actuals","ensemble.forecast")

# absolute Error
ensemble.abs.error = abs(sum(forecastValue.ensemble$actuals,na.rm = T)-sum(forecastValue.ensemble$ensemble.forecast,na.rm = T))/sum(forecastValue.$actuals,na.rm = T)
#0.137732 

# Visulizing the actual vs predicted values
autoplot(cbind(forecastValue$actuals, forecastValue.ensemble$ensemble.forecast)) +
  ggtitle('Ensemble Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','forecasted'),values = c(1,2))
#############################
# Model Evaluation
##############################

autoplot(cbind(ts(forecastValue$actuals),
               ts(forecastValue$tbats.forecast),
               ts(forecastValue$bats.forecast),
               ts(forecastValue$stlm.forecast),
               ts(forecastValue$arima.forecast),
               ts(pred.values),
               ts(model.values$views),
               ts(forecastValue.ensemble$ensemble.forecast))) +
  ggtitle('All 7 model Forecasted values vs Actual Values') +
  ylab('Views (in K)') +
  guides(fill=F) +
  labs(colour='Forecasting') +
  scale_colour_manual(labels=c('actual','TBATS','BATS','STLM','ARIMA','LR','Simple Model','Ensemble'),values = c(1,2,3,4,5,6,7,8))

### from the graph we can conclude that TBATS have perform better than the other model.

################### Coomparing absolute errors ###################

errors <- c(tbats.abs.error,bats.abs.error,stlm.abs.error,arima.abs.error,simplemodel.abs.error,ensemble.abs.error,linear.abs.error)
models <- c('TBATS','BATS','STLM','ARIMA','Simple Model','Ensemble','Linear Model')

df.error <- data.frame(models,errors) %>% arrange(desc(errors))

ggplot(data=df.error, aes(x=models, y=errors, group=1)) +
  geom_line(linetype='solid',color='darkgray',size=1.5)+
  geom_point(size=2) +
  geom_text(aes(label = round(errors, 3)),
            vjust = "outward", hjust = "outward",
            show.legend = FALSE) +
  ggtitle('Absolute Error across Models')
  

