setwd("C:/Users/Marcelo/Desktop/Data/B2WLabs/")
rm(list = ls())
# B2W LABs | Pricing Challenge

# Deliverables:
#  1) Models for Demand Forecasting: The main objective is to create a model to predict the quantity sold for
# each product given a prescribed price. Along with the statistical model, we need metrics, relationships and
# descriptions of these data in order to understand the sales behavior. What does the data tell us? How are
# the different data sources related? Is there a particular competitor that seems more important?
#
# 2) Presentation of the results: we want to know what were the steps and your strategy (approach to the
# problem) during the analysis, even if these may seem wrong. The process you went through and the
# reasoning behind it, is as important as the solutions you found. For this, please prepare a clear and
# objective presentation to explain both your methodology and your results. In case you are selected for the
# interview, you will need to make a 20-minute (max) presentation.

# Loading data sets

sales <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
comp_prices <- read.csv("comp_prices.csv", header = TRUE, stringsAsFactors = FALSE)


str(sales)

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)

library(ggplot2)
library(plyr)
library(dplyr)

# Looking the sales behavior during the entire period
# General overview

sales_by_day <- sales %>%
  group_by(DATE_ORDER) %>%
  summarise(TotalRev = sum(REVENUE))

ggplot(sales_by_day, aes(x = DATE_ORDER, y = TotalRev/1000000))+
  geom_line()

sales_by_day_prod <- sales %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE))

ggplot(sales_by_day_prod, aes(x = DATE_ORDER, y = TotalRev/1000000))+
  geom_line()+
  facet_grid(PROD_ID ~ .)

perc_by_day <- sales_by_day_prod

perc_by_day <- sales %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE)) %>%
  mutate(Perc = TotalRev/sum(TotalRev)*100)

ggplot(perc_by_day, aes(x = DATE_ORDER, y = Perc))+
  geom_line()+
  facet_grid(PROD_ID ~.)

library(data.table)
# Full data set
P1_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P1") %>%
        summarise(RevP1 = sum(REVENUE))
        
fulldata <- merge(sales_by_day, P1_by_day, by = "DATE_ORDER", all = TRUE)
# P2    
P2_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P2") %>%
        summarise(RevP2 = sum(REVENUE))   
        
fulldata <- merge(fulldata, P2_by_day, by = "DATE_ORDER", all = TRUE)
#P3
P3_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P3") %>%
        summarise(RevP3 = sum(REVENUE))   

fulldata <- merge(fulldata, P3_by_day, by = "DATE_ORDER", all = TRUE)
# P4
P4_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P4") %>%
        summarise(RevP4 = sum(REVENUE))   

fulldata <- merge(fulldata, P4_by_day, by = "DATE_ORDER", all = TRUE)

# P5
P5_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P5") %>%
        summarise(RevP5 = sum(REVENUE))   

fulldata <- merge(fulldata, P5_by_day, by = "DATE_ORDER", all = TRUE)
# P6
P6_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P6") %>%
        summarise(RevP6 = sum(REVENUE))   

fulldata <- merge(fulldata, P6_by_day, by = "DATE_ORDER", all = TRUE)
# P7
P7_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P7") %>%
        summarise(RevP7 = sum(REVENUE))   

fulldata <- merge(fulldata, P7_by_day, by = "DATE_ORDER", all = TRUE)
# P8
P8_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P8") %>%
        summarise(RevP8 = sum(REVENUE))   

fulldata <- merge(fulldata, P8_by_day, by = "DATE_ORDER", all = TRUE)

# P9
P9_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        filter(PROD_ID == "P9") %>%
        summarise(RevP9 = sum(REVENUE))   

fulldata <- merge(fulldata, P9_by_day, by = "DATE_ORDER", all = TRUE)

fulldata$PROD_ID.x <- NULL
fulldata$PROD_ID.y <- NULL
fulldata$PROD_ID.x <- NULL
fulldata$PROD_ID.y <- NULL
fulldata$PROD_ID.x <- NULL        
fulldata$PROD_ID.y <- NULL
fulldata$PROD_ID.x <- NULL
fulldata$PROD_ID.y <- NULL
fulldata$PROD_ID <- NULL
        
names(fulldata)

# set NA to 0
fulldata[is.na(fulldata)] <- 0

# Correlation plot
library(corrplot)
corr_Matrix <- cor(fulldata[,2:11])
par(mar = c(5, 4, 4, 2))
corrplot(corr_Matrix, method = "number")

library(highcharter)

sales_by_prod <- sales %>%
  group_by(PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE)) %>%
  mutate(Perc = TotalRev/sum(TotalRev)*100)

# Creating a treemap
hchart(sales_by_prod, "treemap", hcaes(x = PROD_ID, value = Perc, color = TotalRev )) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Participation of the Products in the Total Revenue") %>%
  hc_legend(enabled = TRUE)

# By far P7 is the most important to the company revenue followed by P2, P5 and P8.
# Togheter they are responsible for more than 87% of the Total Revenue.

# Forecasting with ARIMA
# Time series with trends, or with seasonality, are not stationary - the trend 
# and seasonality will affect the value of the time series at different times. 

library(forecast)

sales_totRev <- ts(fulldata$TotalRev, start = 1, end = 287, frequency = 1)
plot.ts(sales_totRev)

sales_ts_TotRev <- forecast(sales_totRev, h = 15)  
sales_ts_TotRev
# residuals
plot.ts(sales_ts_TotRev$residuals)
# error histogram
plotForecastErrors(sales_ts_TotRev$residuals)

# The plot shows that the distribution of forecast errors is roughly 
# centred on zero, and is more or less normally distributed.
# And so it is plausible that the forecast errors are normally
# distributed with mean zero.

# Refining model
auto.arima(sales_ts)
sales_ts_arima <- arima(sales_ts, order = c(2, 0, 1))
sales_ts_fc_2 <- forecast.Arima(sales_ts_arima, h = 10)
sales_ts_fc_2
plot(forecast(sales_ts_fc_2, h=10))
summary(sales_ts_arima)

# residuals
plot.ts(sales_ts_arima$residuals)
# error histogram
plotForecastErrors(sales_ts_arima$residuals)

acf(sales_ts_arima$residuals, lag.max = 20)

# correlogram shows that the sample autocorrelation at lag 13 exceeds the
# significance bounds. However, this is probably due to chance,
# since we would expect one out of 20 sample autocorrelations to 
# exceed the 95% significance bounds.

Box.test(sales_ts_arima$residuals, lag=20, type="Ljung-Box")
# Ljung-Box test is 0.2, indicating that there is little evidence for
# non-zero autocorrelations in the forecast errors for lags 1-20

########################################################################
library(e1071)
# Separating train and test datasets
library(caTools)
sales_ts_nn <- as.data.frame(sales_ts)[,2:11]
split2 = sample.split(sales_ts_nn, SplitRatio = 0.85)
train_nn <- subset(sales_ts_nn, split2 == TRUE)
val_nn <- subset(sales_ts_nn, split2 == FALSE)

model_svm <- svm(log(TotalRev+1) ~ . , data = train_nn, scale = TRUE, kernel = 'radial',
                 cachesize = 800, cross = 5, epsilon = 0.2)

svm_val <- predict(model_svm, val_nn)

error <- log(val_nn$TotalRev+1) - svm_val
RMSE <- rmse(error)
RMSE

plot(log(val_nn$TotalRev+1), svm_val)

tuneResult <- tune(svm, sales_ts ~ . , data = train_nn,scale = TRUE, kernel = 'radial',
                   cachesize = 800, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(tuneResult)
plot(tuneResult)

tuneModel <- tuneResult$best.model
summary(tuneModel)

tuneModelY <- predict(tuneModel, val_nn)

errorTunedModel <- val_nn$sales_ts - svm_val
tunedModelRMSE <-rmse(errorTunedModel)
tunedModelRMSE

plot(val_nn$sales_ts, tuneModelY)
###############################################################################

library(lubridate)

str(comp_prices)
comp_prices$Day <- strptime(comp_prices$DATE_EXTRACTION, "%Y-%m-%d")
comp_prices$hour <- strftime(comp_prices$DATE_EXTRACTION, "%H:%M:%S")

comp_prices$Day <- as.Date(comp_prices$Day)

price_by_day <- comp_prices %>%
        group_by(Day, PROD_ID) %>%
        summarise(MaxPrice = max(COMPETITOR_PRICE),
                  MinPrice = min(COMPETITOR_PRICE),
                  AvgPrice = mean(COMPETITOR_PRICE))

price_prod_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        summarise(OurPrice = sum(REVENUE)/length(PROD_ID))



library(data.table)

Price_data <- merge(price_by_day, price_prod_by_day, by = c("Day", "PROD", all = TRUE)








ggplot(comp_prices, aes(x = Day, y = COMPETITOR_PRICE))+
        geom_line(mapping = aes(color = COMPETITOR))+
        facet_grid( ~ PROD_ID)


##### functions

plotForecastErrors <- function(forecasterrors)
{
        # make a histogram of the forecast errors:
        mybinsize <- IQR(forecasterrors)/4
        mysd   <- sd(forecasterrors)
        mymin  <- min(forecasterrors) - mysd*5
        mymax  <- max(forecasterrors) + mysd*3
        # generate normally distributed data with mean 0 and standard deviation mysd
        mynorm <- rnorm(10000, mean=0, sd=mysd)
        mymin2 <- min(mynorm)
        mymax2 <- max(mynorm)
        if (mymin2 < mymin) { mymin <- mymin2 }
        if (mymax2 > mymax) { mymax <- mymax2 }
        # make a red histogram of the forecast errors, with the normally distributed data overlaid:
        mybins <- seq(mymin, mymax, mybinsize)
        hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
        # freq=FALSE ensures the area under the histogram = 1
        # generate normally distributed data with mean 0 and standard deviation mysd
        myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
        # plot the normal curve as a blue line on top of the histogram of forecast errors:
        points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

rmse <- function(error)
{
        sqrt(mean(error^2))
}
