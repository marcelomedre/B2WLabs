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
P2 <- read.csv("PROD_2.csv", sep = " ", header = TRUE, stringsAsFactors = FALSE)

# Correlation
cor(log(P2$QTY_ORDER), log(P2$Price))

# Inverse correlation, increasing the price, quantity sold decreases
# Plot

plot(log(P2$Price), log(P2$QTY_ORDER))

regp2 <- lm(log(P2$QTY_ORDER) ~ log(P2$Price), data = P2)
plot(log(P2$Price), log(P2$QTY_ORDER))
abline(regp2)

# Model P2

#normalizing data
data_P2 <- P2[,3:5]
maxs <- apply(data_P2, 2, max)
mins <- apply(data_P2, 2, min)
P2_scaled <- as.data.frame(scale(data_P2, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins

plot(P2_scaled$Price, P2_scaled$QTY_ORDER)

names(P2_scaled)

feats <- names(P2_scaled)[2:3]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))

f

library(neuralnet)
library(caTools)
# Split fulldata into train and test
split = sample.split(P2_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P2_scaled, split == TRUE)
val_nn <- subset(P2_scaled, split == FALSE)

nn <- neuralnet(f, train_nn, hidden = 5, linear.output = FALSE)

plot(nn)

# Compute predictions

predicted <- compute(nn, val_nn[2:3])

print(head(predicted$net.result))

results <- as.data.frame(cbind(val_nn, predicted$net.result))

error <- val_nn$QTY_ORDER - predicted$net.result
RMSE <- rmse(error)
RMSE

# r2 validation sample

rdois <- lm(QTY_ORDER ~ predicted$net.result, data = results)
summary(rdois)$r.squared

plot(results$QTY_ORDER, predicted$net.result)
abline(0,1, col = "red")

plot(density(resid(rdois)))

ggplot(results, aes(x = Price, y = QTY_ORDER))+
        geom_point()+
        geom_point(aes(y = predicted$net.result), col = "red")

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
vall_rescaled <- val_nn* rep(b, each = nrow(val_nn)) + rep (a, each = nrow(val_nn))

results_sc <- cbind(predicted$net.result, val_nn[2:3])

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(vall_rescaled, results_sc[1]))
names(results_scaled) <- c("QTY_ORDER", "REVENUE", "PRICE", "ANN_QTY") 

error <- results_scaled$QTY_ORDER - results_scaled$ANN_QTY
RMSE <- rmse(error)
RMSE

# r2 validation sample

rdois <- lm(results_scaled$QTY_ORDER ~ results_scaled$ANN_QTY, data = results_scaled)
summary(rdois)$r.squared

plot(results_scaled$QTY_ORDER ~ results_scaled$ANN_QTY)
abline(0,1, col = "red")

library(ggplot2)
ggplot(results_scaled, aes(x = PRICE, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = ANN_QTY), col = "red")
