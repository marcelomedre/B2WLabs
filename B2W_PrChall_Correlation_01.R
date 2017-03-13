setwd("C:/Users/Marcelo/Desktop/Data Science/B2W Labs/B2WLabs/")

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

# Summarising data set TOTAL QTY_PROD and mean Price of sold products per day

sales_by_weekday_month <- sales %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise_each(funs(sum), QTY_ORDER, REVENUE) %>%
  mutate(Price = REVENUE/QTY_ORDER)
sales_by_weekday_month$QTY_ORDER <- NULL
sales_by_weekday_month$REVENUE <- NULL

temp_out <- split(sales_by_weekday_month, f = sales_by_weekday_month$PROD_ID)

P1_sales_by_day <- temp_out[[1]]
names(P1_sales_by_day) <- c("date", "Prod", "Price")

#*******************************************************************************
# PAY TYPE 1 - COMPETITOR 1
# todos os produtos competidor 1 pagamento 1
temp_CP1_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C1") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_PT_1 <- split(temp_CP1_PT_1, f = temp_CP1_PT_1$PROD_ID)

P1_CP_by_day <- temp_out_PT_1[[1]]
P1_CP_by_day$PROD_ID <- NULL
P1_CP_by_day$COMPETITOR <- NULL
P1_CP_by_day$PAY_TYPE <- NULL

names(P1_CP_by_day) <- c("date", "Price.C1.PT1")
P1_merged <- merge(P1_sales_by_day, P1_CP_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 1
# todos os produtos competidor 1 pagamento 2
temp_CP1_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C1") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP1_PT_2 <- split(temp_CP1_PT_2, f = temp_CP1_PT_2$PROD_ID)

P1_CP_1_by_day <- temp_out_CP1_PT_2[[1]]
P1_CP_1_by_day$PROD_ID <- NULL
P1_CP_1_by_day$COMPETITOR <- NULL
P1_CP_1_by_day$PAY_TYPE <- NULL

names(P1_CP_1_by_day) <- c("date", "Price.C1.PT2")
P1_merged <- merge(P1_merged, P1_CP_1_by_day, by = "date", all = TRUE)
#******************************************************************************
# PAY TYPE 1 - COMPETITOR 2
# todos os produtos competidor 2 pagamento 1
temp_CP2_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C2") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out__CP2_PT_1 <- split(temp_CP2_PT_1, f = temp_CP2_PT_1$PROD_ID)

P1_CP_1_by_day <- temp_out__CP2_PT_1[[1]]
P1_CP_1_by_day$PROD_ID <- NULL
P1_CP_1_by_day$COMPETITOR <- NULL
P1_CP_1_by_day$PAY_TYPE <- NULL

names(P1_CP_1_by_day) <- c("date", "Price.C2.PT1")
P1_merged <- merge(P1_merged, P1_CP_1_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
# todos os produtos competidor 2 pagamento 2
temp_CP2_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C2") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP2_PT_2 <- split(temp_CP2_PT_2, f = temp_CP2_PT_2$PROD_ID)

P1_CP_1_PT_2 <- temp_out_CP2_PT_2[[1]]
P1_CP_1_PT_2$PROD_ID <- NULL
P1_CP_1_PT_2$COMPETITOR <- NULL
P1_CP_1_PT_2$PAY_TYPE <- NULL

names(P1_CP_1_PT_2) <- c("date", "Price.C2.PT2")
P1_merged <- merge(P1_merged, P1_CP_1_PT_2, by = "date", all = TRUE)
#******************************************************************************
# PAY TYPE 1 - COMPETITOR 3
# todos os produtos competidor 3 pagamento 1
temp_CP3_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C3") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out__CP3_PT_1 <- split(temp_CP3_PT_1, f = temp_CP3_PT_1$PROD_ID)

P1_CP_3_by_day <- temp_out__CP3_PT_1[[1]]
P1_CP_3_by_day$PROD_ID <- NULL
P1_CP_3_by_day$COMPETITOR <- NULL
P1_CP_3_by_day$PAY_TYPE <- NULL

names(P1_CP_3_by_day) <- c("date", "Price.C3.PT1")
P1_merged <- merge(P1_merged, P1_CP_3_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
# todos os produtos competidor 3 pagamento 2
temp_CP3_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C3") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP3_PT_2 <- split(temp_CP3_PT_2, f = temp_CP3_PT_2$PROD_ID)

P1_CP_3_PT_2 <- temp_out_CP3_PT_2[[1]]
P1_CP_3_PT_2$PROD_ID <- NULL
P1_CP_3_PT_2$COMPETITOR <- NULL
P1_CP_3_PT_2$PAY_TYPE <- NULL

names(P1_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P1_merged <- merge(P1_merged, P1_CP_3_PT_2, by = "date", all = TRUE)

#******************************************************************************
# PAY TYPE 1 - COMPETITOR 4
# todos os produtos competidor 4 pagamento 1
temp_CP4_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C4") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out__CP4_PT_1 <- split(temp_CP4_PT_1, f = temp_CP4_PT_1$PROD_ID)

P1_CP_4_by_day <- temp_out__CP4_PT_1[[1]]
P1_CP_4_by_day$PROD_ID <- NULL
P1_CP_4_by_day$COMPETITOR <- NULL
P1_CP_4_by_day$PAY_TYPE <- NULL

names(P1_CP_4_by_day) <- c("date", "Price.C4.PT1")
P1_merged <- merge(P1_merged, P1_CP_4_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
# todos os produtos competidor 4 pagamento 2
temp_CP4_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C4") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP4_PT_2 <- split(temp_CP4_PT_2, f = temp_CP4_PT_2$PROD_ID)

P1_CP_4_PT_2 <- temp_out_CP4_PT_2[[1]]
P1_CP_4_PT_2$PROD_ID <- NULL
P1_CP_4_PT_2$COMPETITOR <- NULL
P1_CP_4_PT_2$PAY_TYPE <- NULL

names(P1_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P1_merged <- merge(P1_merged, P1_CP_4_PT_2, by = "date", all = TRUE)

#******************************************************************************
# PAY TYPE 1 - COMPETITOR 5
# todos os produtos competidor 5 pagamento 1
temp_CP5_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C5") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out__CP5_PT_1 <- split(temp_CP5_PT_1, f = temp_CP5_PT_1$PROD_ID)

P1_CP_5_by_day <- temp_out__CP5_PT_1[[1]]
P1_CP_5_by_day$PROD_ID <- NULL
P1_CP_5_by_day$COMPETITOR <- NULL
P1_CP_5_by_day$PAY_TYPE <- NULL

names(P1_CP_5_by_day) <- c("date", "Price.C5.PT1")
P1_merged <- merge(P1_merged, P1_CP_5_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 5
# todos os produtos competidor 5 pagamento 2
temp_CP5_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C5") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP5_PT_2 <- split(temp_CP5_PT_2, f = temp_CP5_PT_2$PROD_ID)

P1_CP_5_PT_2 <- temp_out_CP5_PT_2[[1]]
P1_CP_5_PT_2$PROD_ID <- NULL
P1_CP_5_PT_2$COMPETITOR <- NULL
P1_CP_5_PT_2$PAY_TYPE <- NULL

names(P1_CP_5_PT_2) <- c("date", "Price.C5.PT2")
P1_merged <- merge(P1_merged, P1_CP_5_PT_2, by = "date", all = TRUE)

#******************************************************************************
# PAY TYPE 1 - COMPETITOR 6
# todos os produtos competidor 6 pagamento 1
temp_CP6_PT_1 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "1") %>%
  filter(COMPETITOR == "C6") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out__CP6_PT_1 <- split(temp_CP6_PT_1, f = temp_CP6_PT_1$PROD_ID)

P1_CP_6_by_day <- temp_out__CP6_PT_1[[1]]
P1_CP_6_by_day$PROD_ID <- NULL
P1_CP_6_by_day$COMPETITOR <- NULL
P1_CP_6_by_day$PAY_TYPE <- NULL

names(P1_CP_6_by_day) <- c("date", "Price.C6.PT1")
P1_merged <- merge(P1_merged, P1_CP_6_by_day, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
# todos os produtos competidor 6 pagamento 2
temp_CP6_PT_2 <- comp_prices %>%
  group_by(date, PROD_ID, COMPETITOR, PAY_TYPE) %>%
  filter(PAY_TYPE == "2") %>%
  filter(COMPETITOR == "C6") %>%
  summarise_each(funs(mean), COMPETITOR_PRICE)

temp_out_CP6_PT_2 <- split(temp_CP6_PT_2, f = temp_CP6_PT_2$PROD_ID)

P1_CP_6_PT_2 <- temp_out_CP6_PT_2[[1]]
P1_CP_6_PT_2$PROD_ID <- NULL
P1_CP_6_PT_2$COMPETITOR <- NULL
P1_CP_6_PT_2$PAY_TYPE <- NULL

names(P1_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P1_merged <- merge(P1_merged, P1_CP_6_PT_2, by = "date", all = TRUE)
P1_merged2 <- P1_merged[,-(10:11)] # Removing CP4 
# Removing Rows with NA's
Price_P1 <- P1_merged2[complete.cases(P1_merged2),]
# Removing outliers
library(outliers)
library(corrplot)
Price_P1 <- Price_P1[,3:13]
str(Price_P1)
Price_P1 <- rm.outlier(Price_P1, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P1 <- cor(Price_P1)
#*******************************************************************************
#*******************************************************************************
# PRODUCT 2 - Correlation Matrix
# Product 2
P2_sales_by_day <- temp_out[[2]]
names(P2_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P2_CP_1_PT_1 <- temp_out_PT_1[[2]]
P2_CP_1_PT_1$PROD_ID <- NULL
P2_CP_1_PT_1$COMPETITOR <- NULL
P2_CP_1_PT_1$PAY_TYPE <- NULL

names(P2_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P2_merged <- merge(P2_sales_by_day, P2_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P2_CP_1_PT_2 <- temp_out_CP1_PT_2[[2]]
P2_CP_1_PT_2$PROD_ID <- NULL
P2_CP_1_PT_2$COMPETITOR <- NULL
P2_CP_1_PT_2$PAY_TYPE <- NULL

names(P2_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P2_merged <- merge(P2_merged, P2_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P2_CP_2_PT_2 <- temp_out__CP2_PT_1[[2]]
P2_CP_2_PT_2$PROD_ID <- NULL
P2_CP_2_PT_2$COMPETITOR <- NULL
P2_CP_2_PT_2$PAY_TYPE <- NULL

names(P2_CP_2_PT_2) <- c("date", "Price.C2.PT1")
P2_merged <- merge(P2_merged, P2_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P2_CP_2_PT_2 <- temp_out_CP2_PT_2[[2]]
P2_CP_2_PT_2$PROD_ID <- NULL
P2_CP_2_PT_2$COMPETITOR <- NULL
P2_CP_2_PT_2$PAY_TYPE <- NULL

names(P2_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P2_merged <- merge(P2_merged, P2_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P2_CP_3_PT_1 <- temp_out__CP3_PT_1[[2]]
P2_CP_3_PT_1$PROD_ID <- NULL
P2_CP_3_PT_1$COMPETITOR <- NULL
P2_CP_3_PT_1$PAY_TYPE <- NULL

names(P2_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P2_merged <- merge(P2_merged, P2_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P2_CP_3_PT_2 <- temp_out_CP3_PT_2[[2]]
P2_CP_3_PT_2$PROD_ID <- NULL
P2_CP_3_PT_2$COMPETITOR <- NULL
P2_CP_3_PT_2$PAY_TYPE <- NULL

names(P2_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P2_merged <- merge(P2_merged, P2_CP_3_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 4
P2_CP_4_PT_1 <- temp_out__CP4_PT_1[[1]]
P2_CP_4_PT_1$PROD_ID <- NULL
P2_CP_4_PT_1$COMPETITOR <- NULL
P2_CP_4_PT_1$PAY_TYPE <- NULL

names(P2_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P2_merged <- merge(P2_merged, P2_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P2_CP_4_PT_2 <- temp_out_CP4_PT_2[[1]]
P2_CP_4_PT_2$PROD_ID <- NULL
P2_CP_4_PT_2$COMPETITOR <- NULL
P2_CP_4_PT_2$PAY_TYPE <- NULL

names(P2_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P2_merged <- merge(P2_merged, P2_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 5
P2_CP_5_PT_1 <- temp_out__CP5_PT_1[[2]]
P2_CP_5_PT_1$PROD_ID <- NULL
P2_CP_5_PT_1$COMPETITOR <- NULL
P2_CP_5_PT_1$PAY_TYPE <- NULL

P2_CP_5_PT_1$COMPETITOR_PRICE <- as.numeric(P2_CP_5_PT_1$COMPETITOR_PRICE)

names(P2_CP_5_PT_1) <- c("date", "Price.C5.PT1")
P2_merged <- merge(P2_merged, P2_CP_5_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 5
P2_CP_5_PT_2 <- temp_out_CP5_PT_2[[2]]
P2_CP_5_PT_2$PROD_ID <- NULL
P2_CP_5_PT_2$COMPETITOR <- NULL
P2_CP_5_PT_2$PAY_TYPE <- NULL

names(P2_CP_5_PT_2) <- c("date", "Price.C5.PT2")
P2_merged <- merge(P2_merged, P2_CP_5_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P2_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[2]]
P2_CP_6_by_PT_1$PROD_ID <- NULL
P2_CP_6_by_PT_1$COMPETITOR <- NULL
P2_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P2_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P2_merged <- merge(P2_merged, P2_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P2_CP_6_PT_2 <- temp_out_CP6_PT_2[[2]]
P2_CP_6_PT_2$PROD_ID <- NULL
P2_CP_6_PT_2$COMPETITOR <- NULL
P2_CP_6_PT_2$PAY_TYPE <- NULL

names(P2_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P2_merged <- merge(P2_merged, P2_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P2 <- P2_merged[complete.cases(P2_merged),]
# Removing outliers
Price_P2 <- Price_P2[,3:13]
Price_P2 <- rm.outlier(Price_P2, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P2 <- cor(Price_P2)
corrplot(cor_P2, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 3 - Correlation Matrix
# Product 3
P3_sales_by_day <- temp_out[[3]]
names(P3_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P3_CP_1_PT_1 <- temp_out_PT_1[[3]]
P3_CP_1_PT_1$PROD_ID <- NULL
P3_CP_1_PT_1$COMPETITOR <- NULL
P3_CP_1_PT_1$PAY_TYPE <- NULL

names(P3_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P3_merged <- merge(P3_sales_by_day, P3_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P3_CP_1_PT_2 <- temp_out_CP1_PT_2[[3]]
P3_CP_1_PT_2$PROD_ID <- NULL
P3_CP_1_PT_2$COMPETITOR <- NULL
P3_CP_1_PT_2$PAY_TYPE <- NULL

names(P3_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P3_merged <- merge(P3_merged, P3_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P3_CP_2_PT_1 <- temp_out__CP2_PT_1[[3]]
P3_CP_2_PT_1$PROD_ID <- NULL
P3_CP_2_PT_1$COMPETITOR <- NULL
P3_CP_2_PT_1$PAY_TYPE <- NULL

names(P3_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P3_merged <- merge(P3_merged, P3_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P3_CP_2_PT_2 <- temp_out_CP2_PT_2[[3]]
P3_CP_2_PT_2$PROD_ID <- NULL
P3_CP_2_PT_2$COMPETITOR <- NULL
P3_CP_2_PT_2$PAY_TYPE <- NULL

names(P3_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P3_merged <- merge(P3_merged, P3_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P3_CP_3_PT_1 <- temp_out__CP3_PT_1[[3]]
P3_CP_3_PT_1$PROD_ID <- NULL
P3_CP_3_PT_1$COMPETITOR <- NULL
P3_CP_3_PT_1$PAY_TYPE <- NULL

names(P3_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P3_merged <- merge(P3_merged, P3_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P3_CP_3_PT_2 <- temp_out_CP3_PT_2[[3]]
P3_CP_3_PT_2$PROD_ID <- NULL
P3_CP_3_PT_2$COMPETITOR <- NULL
P3_CP_3_PT_2$PAY_TYPE <- NULL

names(P3_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P3_merged <- merge(P3_merged, P3_CP_3_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 4
P3_CP_4_PT_1 <- temp_out__CP4_PT_1[[2]]
P3_CP_4_PT_1$PROD_ID <- NULL
P3_CP_4_PT_1$COMPETITOR <- NULL
P3_CP_4_PT_1$PAY_TYPE <- NULL

names(P3_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P3_merged <- merge(P3_merged, P3_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P3_CP_4_PT_2 <- temp_out_CP4_PT_2[[2]]
P3_CP_4_PT_2$PROD_ID <- NULL
P3_CP_4_PT_2$COMPETITOR <- NULL
P3_CP_4_PT_2$PAY_TYPE <- NULL

names(P3_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P3_merged <- merge(P3_merged, P3_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 5
P3_CP_5_PT_1 <- temp_out__CP5_PT_1[[3]]
P3_CP_5_PT_1$PROD_ID <- NULL
P3_CP_5_PT_1$COMPETITOR <- NULL
P3_CP_5_PT_1$PAY_TYPE <- NULL

names(P3_CP_5_PT_1) <- c("date", "Price.C5.PT1")
P3_merged <- merge(P3_merged, P3_CP_5_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 5
P3_CP_5_PT_2 <- temp_out_CP5_PT_2[[3]]
P3_CP_5_PT_2$PROD_ID <- NULL
P3_CP_5_PT_2$COMPETITOR <- NULL
P3_CP_5_PT_2$PAY_TYPE <- NULL

names(P3_CP_5_PT_2) <- c("date", "Price.C5.PT2")
P3_merged <- merge(P3_merged, P3_CP_5_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P3_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[3]]
P3_CP_6_by_PT_1$PROD_ID <- NULL
P3_CP_6_by_PT_1$COMPETITOR <- NULL
P3_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P3_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P3_merged <- merge(P3_merged, P3_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P3_CP_6_PT_2 <- temp_out_CP6_PT_2[[3]]
P3_CP_6_PT_2$PROD_ID <- NULL
P3_CP_6_PT_2$COMPETITOR <- NULL
P3_CP_6_PT_2$PAY_TYPE <- NULL

names(P3_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P3_merged <- merge(P3_merged, P3_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P3 <- P3_merged[complete.cases(P3_merged),]
# Removing outliers
Price_P3 <- Price_P3[,3:13]
Price_P3 <- rm.outlier(Price_P3, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P3 <- cor(Price_P3)
corrplot(cor_P3, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 4 - Correlation Matrix
# Product 4
P4_sales_by_day <- temp_out[[4]]
names(P4_sales_by_day) <- c("date", "Prod", "Price")

# PAY TYPE 1 - COMPETITOR 4
P4_CP_4_PT_1 <- temp_out__CP4_PT_1[[3]]
P4_CP_4_PT_1$PROD_ID <- NULL
P4_CP_4_PT_1$COMPETITOR <- NULL
P4_CP_4_PT_1$PAY_TYPE <- NULL

names(P4_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P4_merged <- merge(P4_sales_by_day, P4_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P4_CP_4_PT_2 <- temp_out_CP4_PT_2[[3]]
P4_CP_4_PT_2$PROD_ID <- NULL
P4_CP_4_PT_2$COMPETITOR <- NULL
P4_CP_4_PT_2$PAY_TYPE <- NULL

names(P4_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P4_merged <- merge(P4_merged, P4_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P4_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[4]]
P4_CP_6_by_PT_1$PROD_ID <- NULL
P4_CP_6_by_PT_1$COMPETITOR <- NULL
P4_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P4_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P4_merged <- merge(P4_merged, P4_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P4_CP_6_PT_2 <- temp_out_CP6_PT_2[[4]]
P4_CP_6_PT_2$PROD_ID <- NULL
P4_CP_6_PT_2$COMPETITOR <- NULL
P4_CP_6_PT_2$PAY_TYPE <- NULL

names(P4_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P4_merged <- merge(P4_merged, P4_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P4 <- P4_merged[complete.cases(P4_merged),]
# Removing outliers
Price_P4 <- Price_P4[,3:7]
Price_P4 <- rm.outlier(Price_P4, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P4 <- cor(Price_P4)
corrplot(cor_P4, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 5 - Correlation Matrix
# Product 5
P5_sales_by_day <- temp_out[[5]]
names(P5_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P5_CP_1_PT_1 <- temp_out_PT_1[[4]]
P5_CP_1_PT_1$PROD_ID <- NULL
P5_CP_1_PT_1$COMPETITOR <- NULL
P5_CP_1_PT_1$PAY_TYPE <- NULL

names(P5_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P5_merged <- merge(P5_sales_by_day, P5_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P5_CP_1_PT_2 <- temp_out_CP1_PT_2[[4]]
P5_CP_1_PT_2$PROD_ID <- NULL
P5_CP_1_PT_2$COMPETITOR <- NULL
P5_CP_1_PT_2$PAY_TYPE <- NULL

names(P5_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P5_merged <- merge(P5_merged, P5_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P5_CP_2_PT_1 <- temp_out__CP2_PT_1[[4]]
P5_CP_2_PT_1$PROD_ID <- NULL
P5_CP_2_PT_1$COMPETITOR <- NULL
P5_CP_2_PT_1$PAY_TYPE <- NULL

names(P5_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P5_merged <- merge(P5_merged, P5_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P5_CP_2_PT_2 <- temp_out_CP2_PT_2[[4]]
P5_CP_2_PT_2$PROD_ID <- NULL
P5_CP_2_PT_2$COMPETITOR <- NULL
P5_CP_2_PT_2$PAY_TYPE <- NULL

names(P5_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P5_merged <- merge(P5_merged, P5_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P5_CP_3_PT_1 <- temp_out__CP3_PT_1[[4]]
P5_CP_3_PT_1$PROD_ID <- NULL
P5_CP_3_PT_1$COMPETITOR <- NULL
P5_CP_3_PT_1$PAY_TYPE <- NULL

names(P5_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P5_merged <- merge(P5_merged, P5_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P5_CP_3_PT_2 <- temp_out_CP3_PT_2[[4]]
P5_CP_3_PT_2$PROD_ID <- NULL
P5_CP_3_PT_2$COMPETITOR <- NULL
P5_CP_3_PT_2$PAY_TYPE <- NULL

names(P5_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P5_merged <- merge(P5_merged, P5_CP_3_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P5 <- P5_merged[complete.cases(P5_merged),]
# Removing outliers
Price_P5 <- Price_P5[,3:9]
Price_P5 <- rm.outlier(Price_P5, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P5 <- cor(Price_P5)
corrplot(cor_P5, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 6 - Correlation Matrix
# Product 6
P6_sales_by_day <- temp_out[[6]]
names(P6_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P6_CP_1_PT_1 <- temp_out_PT_1[[5]]
P6_CP_1_PT_1$PROD_ID <- NULL
P6_CP_1_PT_1$COMPETITOR <- NULL
P6_CP_1_PT_1$PAY_TYPE <- NULL

names(P6_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P6_merged <- merge(P6_sales_by_day, P6_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P6_CP_1_PT_2 <- temp_out_CP1_PT_2[[5]]
P6_CP_1_PT_2$PROD_ID <- NULL
P6_CP_1_PT_2$COMPETITOR <- NULL
P6_CP_1_PT_2$PAY_TYPE <- NULL

names(P6_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P6_merged <- merge(P6_merged, P6_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P6_CP_2_PT_1 <- temp_out__CP2_PT_1[[5]]
P6_CP_2_PT_1$PROD_ID <- NULL
P6_CP_2_PT_1$COMPETITOR <- NULL
P6_CP_2_PT_1$PAY_TYPE <- NULL

names(P6_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P6_merged <- merge(P6_merged, P6_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P6_CP_2_PT_2 <- temp_out_CP2_PT_2[[5]]
P6_CP_2_PT_2$PROD_ID <- NULL
P6_CP_2_PT_2$COMPETITOR <- NULL
P6_CP_2_PT_2$PAY_TYPE <- NULL

names(P6_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P6_merged <- merge(P6_merged, P6_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P6_CP_3_PT_1 <- temp_out__CP3_PT_1[[5]]
P6_CP_3_PT_1$PROD_ID <- NULL
P6_CP_3_PT_1$COMPETITOR <- NULL
P6_CP_3_PT_1$PAY_TYPE <- NULL

names(P6_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P6_merged <- merge(P6_merged, P6_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 4
P6_CP_4_PT_1 <- temp_out__CP4_PT_1[[4]]
P6_CP_4_PT_1$PROD_ID <- NULL
P6_CP_4_PT_1$COMPETITOR <- NULL
P6_CP_4_PT_1$PAY_TYPE <- NULL

names(P6_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P6_merged <- merge(P6_merged, P6_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P6_CP_4_PT_2 <- temp_out_CP4_PT_2[[4]]
P6_CP_4_PT_2$PROD_ID <- NULL
P6_CP_4_PT_2$COMPETITOR <- NULL
P6_CP_4_PT_2$PAY_TYPE <- NULL

names(P6_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P6_merged <- merge(P6_merged, P6_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P6_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[5]]
P6_CP_6_by_PT_1$PROD_ID <- NULL
P6_CP_6_by_PT_1$COMPETITOR <- NULL
P6_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P6_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P6_merged <- merge(P6_merged, P6_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P6_CP_6_PT_2 <- temp_out_CP6_PT_2[[5]]
P6_CP_6_PT_2$PROD_ID <- NULL
P6_CP_6_PT_2$COMPETITOR <- NULL
P6_CP_6_PT_2$PAY_TYPE <- NULL

names(P6_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P6_merged <- merge(P6_merged, P6_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P6 <- P6_merged[complete.cases(P6_merged),]
# Removing outliers
Price_P6 <- Price_P6[,3:12]
Price_P6 <- rm.outlier(Price_P6, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P6 <- cor(Price_P6)
corrplot(cor_P6, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 7 - Correlation Matrix
# Product 7
P7_sales_by_day <- temp_out[[7]]
names(P7_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P7_CP_1_PT_1 <- temp_out_PT_1[[6]]
P7_CP_1_PT_1$PROD_ID <- NULL
P7_CP_1_PT_1$COMPETITOR <- NULL
P7_CP_1_PT_1$PAY_TYPE <- NULL

names(P7_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P7_merged <- merge(P7_sales_by_day, P7_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P7_CP_1_PT_2 <- temp_out_CP1_PT_2[[6]]
P7_CP_1_PT_2$PROD_ID <- NULL
P7_CP_1_PT_2$COMPETITOR <- NULL
P7_CP_1_PT_2$PAY_TYPE <- NULL

names(P7_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P7_merged <- merge(P7_merged, P7_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P7_CP_2_PT_1 <- temp_out__CP2_PT_1[[6]]
P7_CP_2_PT_1$PROD_ID <- NULL
P7_CP_2_PT_1$COMPETITOR <- NULL
P7_CP_2_PT_1$PAY_TYPE <- NULL

names(P7_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P7_merged <- merge(P7_merged, P7_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P7_CP_2_PT_2 <- temp_out_CP2_PT_2[[6]]
P7_CP_2_PT_2$PROD_ID <- NULL
P7_CP_2_PT_2$COMPETITOR <- NULL
P7_CP_2_PT_2$PAY_TYPE <- NULL

names(P7_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P7_merged <- merge(P7_merged, P7_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P7_CP_3_PT_1 <- temp_out__CP3_PT_1[[6]]
P7_CP_3_PT_1$PROD_ID <- NULL
P7_CP_3_PT_1$COMPETITOR <- NULL
P7_CP_3_PT_1$PAY_TYPE <- NULL

names(P7_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P7_merged <- merge(P7_merged, P7_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P7_CP_3_PT_2 <- temp_out_CP3_PT_2[[6]]
P7_CP_3_PT_2$PROD_ID <- NULL
P7_CP_3_PT_2$COMPETITOR <- NULL
P7_CP_3_PT_2$PAY_TYPE <- NULL

names(P7_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P7_merged <- merge(P7_merged, P7_CP_3_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 4
P7_CP_4_PT_1 <- temp_out__CP4_PT_1[[5]]
P7_CP_4_PT_1$PROD_ID <- NULL
P7_CP_4_PT_1$COMPETITOR <- NULL
P7_CP_4_PT_1$PAY_TYPE <- NULL

names(P7_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P7_merged <- merge(P7_merged, P7_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P7_CP_4_PT_2 <- temp_out_CP4_PT_2[[5]]
P7_CP_4_PT_2$PROD_ID <- NULL
P7_CP_4_PT_2$COMPETITOR <- NULL
P7_CP_4_PT_2$PAY_TYPE <- NULL

names(P7_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P7_merged <- merge(P7_merged, P7_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 5
P7_CP_5_PT_1 <- temp_out__CP5_PT_1[[5]]
P7_CP_5_PT_1$PROD_ID <- NULL
P7_CP_5_PT_1$COMPETITOR <- NULL
P7_CP_5_PT_1$PAY_TYPE <- NULL

names(P7_CP_5_PT_1) <- c("date", "Price.C5.PT1")
P7_merged <- merge(P7_merged, P7_CP_5_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 5
P7_CP_5_PT_2 <- temp_out_CP5_PT_2[[5]]
P7_CP_5_PT_2$PROD_ID <- NULL
P7_CP_5_PT_2$COMPETITOR <- NULL
P7_CP_5_PT_2$PAY_TYPE <- NULL

names(P7_CP_5_PT_2) <- c("date", "Price.C5.PT2")
P7_merged <- merge(P7_merged, P7_CP_5_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P7_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[6]]
P7_CP_6_by_PT_1$PROD_ID <- NULL
P7_CP_6_by_PT_1$COMPETITOR <- NULL
P7_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P7_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P7_merged <- merge(P7_merged, P7_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P7_CP_6_PT_2 <- temp_out_CP6_PT_2[[6]]
P7_CP_6_PT_2$PROD_ID <- NULL
P7_CP_6_PT_2$COMPETITOR <- NULL
P7_CP_6_PT_2$PAY_TYPE <- NULL

names(P7_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P7_merged <- merge(P7_merged, P7_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P7 <- P7_merged[complete.cases(P7_merged),]
# Removing outliers
Price_P7 <- Price_P7[,3:13]
Price_P7 <- rm.outlier(Price_P7, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P7 <- cor(Price_P7)
corrplot(cor_P7, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 8 - Correlation Matrix
# Product 8
P8_sales_by_day <- temp_out[[8]]
names(P8_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P8_CP_1_PT_1 <- temp_out_PT_1[[7]]
P8_CP_1_PT_1$PROD_ID <- NULL
P8_CP_1_PT_1$COMPETITOR <- NULL
P8_CP_1_PT_1$PAY_TYPE <- NULL

names(P8_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P8_merged <- merge(P8_sales_by_day, P8_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P8_CP_1_PT_2 <- temp_out_CP1_PT_2[[7]]
P8_CP_1_PT_2$PROD_ID <- NULL
P8_CP_1_PT_2$COMPETITOR <- NULL
P8_CP_1_PT_2$PAY_TYPE <- NULL

names(P8_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P8_merged <- merge(P8_merged, P8_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P8_CP_2_PT_1 <- temp_out__CP2_PT_1[[7]]
P8_CP_2_PT_1$PROD_ID <- NULL
P8_CP_2_PT_1$COMPETITOR <- NULL
P8_CP_2_PT_1$PAY_TYPE <- NULL

names(P8_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P8_merged <- merge(P8_merged, P8_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P8_CP_2_PT_2 <- temp_out_CP2_PT_2[[7]]
P8_CP_2_PT_2$PROD_ID <- NULL
P8_CP_2_PT_2$COMPETITOR <- NULL
P8_CP_2_PT_2$PAY_TYPE <- NULL

names(P8_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P8_merged <- merge(P8_merged, P8_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P8_CP_3_PT_1 <- temp_out__CP3_PT_1[[7]]
P8_CP_3_PT_1$PROD_ID <- NULL
P8_CP_3_PT_1$COMPETITOR <- NULL
P8_CP_3_PT_1$PAY_TYPE <- NULL

names(P8_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P8_merged <- merge(P8_merged, P8_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P8_CP_3_PT_2 <- temp_out_CP3_PT_2[[7]]
P8_CP_3_PT_2$PROD_ID <- NULL
P8_CP_3_PT_2$COMPETITOR <- NULL
P8_CP_3_PT_2$PAY_TYPE <- NULL

names(P8_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P8_merged <- merge(P8_merged, P8_CP_3_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 4
P8_CP_4_PT_1 <- temp_out__CP4_PT_1[[6]]
P8_CP_4_PT_1$PROD_ID <- NULL
P8_CP_4_PT_1$COMPETITOR <- NULL
P8_CP_4_PT_1$PAY_TYPE <- NULL

names(P8_CP_4_PT_1) <- c("date", "Price.C4.PT1")
P8_merged <- merge(P8_merged, P8_CP_4_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 4
P8_CP_4_PT_2 <- temp_out_CP4_PT_2[[6]]
P8_CP_4_PT_2$PROD_ID <- NULL
P8_CP_4_PT_2$COMPETITOR <- NULL
P8_CP_4_PT_2$PAY_TYPE <- NULL

names(P8_CP_4_PT_2) <- c("date", "Price.C4.PT2")
P8_merged <- merge(P8_merged, P8_CP_4_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P8_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[7]]
P8_CP_6_by_PT_1$PROD_ID <- NULL
P8_CP_6_by_PT_1$COMPETITOR <- NULL
P8_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P8_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P8_merged <- merge(P8_merged, P8_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P8_CP_6_PT_2 <- temp_out_CP6_PT_2[[7]]
P8_CP_6_PT_2$PROD_ID <- NULL
P8_CP_6_PT_2$COMPETITOR <- NULL
P8_CP_6_PT_2$PAY_TYPE <- NULL

names(P8_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P8_merged <- merge(P8_merged, P8_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P8 <- P8_merged[complete.cases(P8_merged),]
# Removing outliers
Price_P8 <- Price_P8[,3:13]
Price_P8 <- rm.outlier(Price_P8, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P8 <- cor(Price_P8)
corrplot(cor_P8, method = "number")

#*******************************************************************************
#*******************************************************************************
# PRODUCT 9 - Correlation Matrix
# Product 9
P9_sales_by_day <- temp_out[[9]]
names(P9_sales_by_day) <- c("date", "Prod", "Price")
# PAY TYPE 1 - COMPETITOR 1
P9_CP_1_PT_1 <- temp_out_PT_1[[8]]
P9_CP_1_PT_1$PROD_ID <- NULL
P9_CP_1_PT_1$COMPETITOR <- NULL
P9_CP_1_PT_1$PAY_TYPE <- NULL

names(P9_CP_1_PT_1) <- c("date", "Price.C1.PT1")
P9_merged <- merge(P9_sales_by_day, P9_CP_1_PT_1, by = "date", all = TRUE)
# PAY TYPE 2 - COMPETITOR 1
P9_CP_1_PT_2 <- temp_out_CP1_PT_2[[8]]
P9_CP_1_PT_2$PROD_ID <- NULL
P9_CP_1_PT_2$COMPETITOR <- NULL
P9_CP_1_PT_2$PAY_TYPE <- NULL

names(P9_CP_1_PT_2) <- c("date", "Price.C1.PT2")
P9_merged <- merge(P9_merged, P9_CP_1_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 2
P9_CP_2_PT_1 <- temp_out__CP2_PT_1[[8]]
P9_CP_2_PT_1$PROD_ID <- NULL
P9_CP_2_PT_1$COMPETITOR <- NULL
P9_CP_2_PT_1$PAY_TYPE <- NULL

names(P9_CP_2_PT_1) <- c("date", "Price.C2.PT1")
P9_merged <- merge(P9_merged, P9_CP_2_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 2
P9_CP_2_PT_2 <- temp_out_CP2_PT_2[[8]]
P9_CP_2_PT_2$PROD_ID <- NULL
P9_CP_2_PT_2$COMPETITOR <- NULL
P9_CP_2_PT_2$PAY_TYPE <- NULL

names(P9_CP_2_PT_2) <- c("date", "Price.C2.PT2")
P9_merged <- merge(P9_merged, P9_CP_2_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 3
P9_CP_3_PT_1 <- temp_out__CP3_PT_1[[8]]
P9_CP_3_PT_1$PROD_ID <- NULL
P9_CP_3_PT_1$COMPETITOR <- NULL
P9_CP_3_PT_1$PAY_TYPE <- NULL

names(P9_CP_3_PT_1) <- c("date", "Price.C3.PT1")
P9_merged <- merge(P9_merged, P9_CP_3_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 3
P9_CP_3_PT_2 <- temp_out_CP3_PT_2[[8]]
P9_CP_3_PT_2$PROD_ID <- NULL
P9_CP_3_PT_2$COMPETITOR <- NULL
P9_CP_3_PT_2$PAY_TYPE <- NULL

names(P9_CP_3_PT_2) <- c("date", "Price.C3.PT2")
P9_merged <- merge(P9_merged, P9_CP_3_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 5
P9_CP_5_PT_1 <- temp_out__CP5_PT_1[[7]]
P9_CP_5_PT_1$PROD_ID <- NULL
P9_CP_5_PT_1$COMPETITOR <- NULL
P9_CP_5_PT_1$PAY_TYPE <- NULL

names(P9_CP_5_PT_1) <- c("date", "Price.C5.PT1")
P9_merged <- merge(P9_merged, P9_CP_5_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 5
P9_CP_5_PT_2 <- temp_out_CP5_PT_2[[7]]
P9_CP_5_PT_2$PROD_ID <- NULL
P9_CP_5_PT_2$COMPETITOR <- NULL
P9_CP_5_PT_2$PAY_TYPE <- NULL

names(P9_CP_5_PT_2) <- c("date", "Price.C5.PT2")
P9_merged <- merge(P9_merged, P9_CP_5_PT_2, by = "date", all = TRUE)

# PAY TYPE 1 - COMPETITOR 6
P9_CP_6_by_PT_1 <- temp_out__CP6_PT_1[[8]]
P9_CP_6_by_PT_1$PROD_ID <- NULL
P9_CP_6_by_PT_1$COMPETITOR <- NULL
P9_CP_6_by_PT_1$PAY_TYPE <- NULL

names(P9_CP_6_by_PT_1) <- c("date", "Price.C6.PT1")
P9_merged <- merge(P9_merged, P9_CP_6_by_PT_1, by = "date", all = TRUE)

# PAY TYPE 2 - COMPETITOR 6
P9_CP_6_PT_2 <- temp_out_CP6_PT_2[[8]]
P9_CP_6_PT_2$PROD_ID <- NULL
P9_CP_6_PT_2$COMPETITOR <- NULL
P9_CP_6_PT_2$PAY_TYPE <- NULL

names(P9_CP_6_PT_2) <- c("date", "Price.C6.PT2")
P9_merged <- merge(P9_merged, P9_CP_6_PT_2, by = "date", all = TRUE)

# Removing Rows with NA's
Price_P9 <- P9_merged[complete.cases(P9_merged),]
# Removing outliers
Price_P9 <- Price_P9[,3:13]
Price_P9 <- rm.outlier(Price_P9, fill = TRUE, median = FALSE, opposite = FALSE)
cor_P9 <- cor(Price_P9)
corrplot(cor_P9, method = "number")
