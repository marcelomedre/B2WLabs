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
P3 <- read.csv("PROD_3.csv", sep = " ", header = TRUE, stringsAsFactors = FALSE)

# Correlation
cor(P3$QTY_ORDER, P3$Price)

# Inverse correlation, increasing the price, quantity sold decreases
# Plot

plot(log(P3$Price), log(P3$QTY_ORDER))

regp3 <- lm(log(P3$QTY_ORDER) ~ log(P3$Price), data = P3)
plot(log(P3$Price), log(P3$QTY_ORDER))
abline(regp3)

# Model P2
data_P3 <- P3[,3:5]

library(caTools)
# Split fulldata into train and test
split = sample.split(data_P3$Price, SplitRatio = 0.80)
train <- subset(data_P3, split == TRUE)
val <- subset(data_P3, split == FALSE)

library(e1071)

model_svm <- tune(svm, QTY_ORDER ~ ., data = train, scale = TRUE, kernel = 'radial',
                  cachesize = 1800, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(model_svm)
plot(model_svm)

summary(model_svm)
model_P3 <- model_svm$best.model

results <- predict(model_P3, val)

errorTunedModel <- val$Price - results
tunedModelRMSE <-rmse(errorTunedModel)
tunedModelRMSE

# r2 validation sample

results <- as.data.frame(cbind(val, results))

rdois <- lm(QTY_ORDER ~ results, data = results)
summary(rdois)$r.squared

plot(results$QTY_ORDER, results$results)
abline(0,1, col = "red")

plot(density(resid(rdois)))

library(ggplot2)
ggplot(results, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = results), col = "red")




