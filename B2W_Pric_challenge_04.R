setwd("C:/Users/Marcelo/Desktop/Data/B2WLabs/")

rm(list = ls())

# Function

rmse <- function(error)
{
        sqrt(mean(error^2))
}
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

library(lubridate)
library(ggplot2)
library(plyr)
library(data.table)
library(dplyr)
library(caTools)

# Loading data sets

sales <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
comp_prices <- read.csv("comp_prices.csv", header = TRUE, stringsAsFactors = FALSE)

str(sales)
head(sales)
tail(sales)

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)
sales$day <- as.numeric(as.factor(wday(sales$DATE_ORDER)))
sales$month <- as.numeric(as.factor(month(sales$DATE_ORDER)))
sales$PROD_PRICE <- sales$REVENUE/sales$QTY_ORDER

# Summarising data set TOTAL QTY_PROD and mean Price of sold products per day
# Produto 1
P1_by_weekday_month <- sales %>%
        group_by(month, day, PROD_ID) %>%
        filter(PROD_ID == "P1") %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot(P1_Price_by_weekday_month, aes(x = day, y = QTY_ORDER))+
        geom_point(data = P1_Price_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
        geom_smooth()+
        geom_line(data = P1_Price_by_weekday_month, aes(group = month, color = factor(month)))+
        xlab("Dia da Semana")+
        ylab("Quantidade vendida Produto 1")
# by month
P1_by_month <- sales %>%
        group_by(month, PROD_ID) %>%
        filter(PROD_ID == "P1") %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot(P1_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
        geom_point(size = 3)+
        xlab("Mês")+
        ylab("Quantidade vendida Produto 1")
# Tendência em ter preços menores no começo do ano, maiores no fim do ano, grosso modo
# as vendas caem ao longo do ano.


#### Exploratory data Analysis Comp_prices

comp_prices$date <- as.Date(comp_prices$DATE_EXTRACTION)
comp_prices$day <- as.numeric(as.factor(wday(comp_prices$date)))
comp_prices$month <- as.numeric(as.factor(month(comp_prices$date)))
comp_prices$time <- strftime(comp_prices$DATE_EXTRACTION, "%H:%M:%S")

# Produto 1 - price
P1_Price_by_month <- comp_prices %>%
        group_by(month, PROD_ID, COMPETITOR, PAY_TYPE) %>%
        filter(PROD_ID == "P1") %>%
        summarise(AVG_Price = mean(COMPETITOR_PRICE))

out <- P1_Price_by_month$AVG_Price < 2000

P1_Price_by_month <- subset(P1_Price_by_month, out ==TRUE)


ggplot()+
        geom_line(data = P1_Price_by_month, 
                  aes(x = month, y = AVG_Price,
                      color = factor(COMPETITOR)), size = 1)+
        geom_point(data = P1_by_month, aes(x = month, y = Price))+
        xlab("Mês")+
        ylab("Price")
# Preço do Produto 1 tende a acompanhar o Preço do Competidores 3 e 5 ao longo dos meses
temp_P1_1 <- comp_prices %>%
        group_by(month, PROD_ID, COMPETITOR, PAY_TYPE) %>%
        filter(COMPETITOR == "C1") %>%
        filter(PAY_TYPE == "1") %>%
        filter(PROD_ID == "P1") %>%
        summarise(AVG_Price = mean(COMPETITOR_PRICE))

P1_merged <- merge(P1_by_month, temp_P1_1, by = "month", all = TRUE)
P1_merged[is.na(P1_merged)] <- 0
