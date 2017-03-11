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

# Loading data sets

sales <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
comp_prices <- read.csv("comp_prices.csv", header = TRUE, stringsAsFactors = FALSE)

str(sales)
head(sales)
tail(sales)

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)

library(ggplot2)
library(plyr)
library(data.table)
library(dplyr)

sales$PROD_PRICE <- sales$REVENUE/sales$QTY_ORDER

# Summarising data set TOTAL QTY_PROD and mean Price of sold products per day
Price_by_day <- sales %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        #filter(PROD_ID == "P1") %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

# Taking a look
ggplot(Price_by_day, aes(x = Price, y = QTY_ORDER))+
        geom_point()+
        facet_grid(~ PROD_ID, scales = "free")

# Modeling quantity sold for each product based on the price
# P1
library(corrplot)
P1 <- Price_by_day %>%
        filter(PROD_ID == "P1")

summary(P1)
par(mfrow = c(1, 2))
boxplot(P1$QTY_ORDER)
boxplot(P1$Price)

# Correlation
cor(P1$QTY_ORDER, P1$Price)
# Inverse correlation, increasing the price, quantity sold decreases
regp1 <- lm(P1$QTY_ORDER ~ poly(log(P1$Price),3), data = P1)
plot(log(P1$Price), P1$QTY_ORDER)
lines(new.x, predict(regp1, data.frame(x=new.x)), col="light blue", lty=4)

summary(regp1)
write.table(P1, file = "Prod_1.csv", col.names = TRUE, row.names = FALSE)

# Model P1
library(e1071)
library(caTools)
set.seed(123)
# Split fulldata into train and test
P1$Price <- log(P1$Price) # calculating log of Price
split = sample.split(P1$Price, SplitRatio = 0.75)
train_nn <- subset(P1, split == TRUE)
val_nn <- subset(P1, split == FALSE)

model_P1 <- tune(svm, QTY_ORDER ~ Price, data = P1, scale = TRUE, kernel = "radial",
                 cachesize = 800, ranges = list(epsilon = seq(0,0.4,0.01), cost = 2^(2:9)))

print(model_P1)
plot(model_P1)

model_P1t <- model_P1$best.model
summary(model_P1t)

svm_val <- predict(model_P1t, val_nn)

error <- val_nn$QTY_ORDER - svm_val
RMSE <- rmse(error)
RMSE

res_P1 <- as.data.frame(cbind(val_nn$QTY_ORDER, svm_val))

# r2 validation sample

rdois <- lm(V1 ~ svm_val, data = res_P1)
summary(rdois)$r.squared

plot(res_P1$V1, res_P1$svm_val)


