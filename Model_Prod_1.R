setwd("C:/Users/Marcelo/Desktop/Data Science/B2W Labs/B2WLabs/")

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
P1_sales_by_weekday_month <- read.csv("P1_sales_by_weekday_month.csv", header = TRUE, stringsAsFactors = FALSE)

P1_sales_by_weekday_month$PROD_ID <- NULL

ggplot(P1_sales_by_weekday_month, aes(x = 1:63, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 1")

plot(P1_sales_by_weekday_month$Price, P1_sales_by_weekday_month$QTY_ORDER)
cor(P1_sales_by_weekday_month$Price, P1_sales_by_weekday_month$QTY_ORDER)
#correlation = -0.643

regP1 <- lm(QTY_ORDER ~ Price, data = P1_sales_by_weekday_month)
summary(regP1)
# r^2 = 0.40

plot(P1_sales_by_weekday_month$Price, P1_sales_by_weekday_month$QTY_ORDER)
abline(regP1, col = "blue")



#normalizing data
data_P2 <- P1_sales_by_weekday_month[,-(4)] # removing Revenue column
maxs <- apply(data_P2, 2, max)
mins <- apply(data_P2, 2, min)
P2_scaled <- as.data.frame(scale(data_P2, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins

plot(P2_scaled$Price, P2_scaled$QTY_ORDER)

names(P2_scaled)

feats <- names(P2_scaled)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))

f

library(neuralnet)
library(caTools)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P2_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P2_scaled, split == TRUE)
val_nn <- subset(P2_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL


library(car)
library(caret)
library(nnet)
my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
P1.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                tuneGrid = my.grid, trace = F, linout = 1) 

model <- P1.fit$bestTune

predicted <- predict(P1.fit, newdata = val_nn)
prestige.rmse <- sqrt(mean((predicted - val_resp)^2))
prestige.rmse
results <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot(results, aes(x = Price, y = val_resp))+
  geom_point(size = 4)+
  geom_point(aes(y = predicted), col = "red")


#******************************************************************

nn <- neuralnet(f, train_nn, rep = 50, hidden = 5, threshold = 0.0001,
                linear.output = FALSE)

plot(nn)

# Compute predictions

predicted <- compute(nn, val_nn)

print(head(predicted$net.result))

results <- as.data.frame(cbind(val_nn, val_resp, predicted$net.result))

error <- val_resp - predicted$net.result
RMSE <- rmse(error)
RMSE

# r2 validation sample

rdois <- lm(val_resp ~ predicted$net.result, data = results)
summary(rdois)$r.squared
#r^2 = 0.617

plot(results$val_resp, predicted$net.result)
abline(0,1, col = "red")

ggplot(results, aes(x = Price, y = val_resp))+
        geom_point(size = 4)+
        geom_point(aes(y = predicted$net.result), col = "red")
#*******************************************************************************
# SVM Model
library(e1071)
# Split fulldata into train and test
set.seed(12345)
split = sample.split(data_P2$Price, SplitRatio = 0.75)
train <- subset(data_P2, split == TRUE)
val <- subset(data_P2, split == FALSE)

model_svm <- svm(f, data = train, scale = TRUE, kernel = 'radial',
                     cachesize = 1600, cross = 5, epsilon = 0.2)

svm_val <- predict(model_svm, val)

results_svm <- as.data.frame(cbind(val, svm_val))

error <- results_svm$QTY_ORDER - svm_val
RMSE <- rmse(error)
RMSE

# r2 validation sample

rdois <- lm(QTY_ORDER ~ svm_val, data = results_svm)
summary(rdois)$r.squared
#r^2 = 0.60

plot(results_svm$QTY_ORDER, results_svm$svm_val)
abline(0,1, col = "red")
abline(rdois, col = "blue")

ggplot(results_svm, aes(x = Price, y = QTY_ORDER))+
  geom_point(size = 4)+
  geom_point(aes(y = results_svm$svm_val), col = "red")


# Refining model

model_svm <- tune(svm, f, data = train, scale = TRUE, kernel = 'radial',
                  cachesize = 2000, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(model_svm)
plot(model_svm)

model_svm <- tune(svm, f, data = train, scale = TRUE, kernel = 'radial',
                  cachesize = 2000, ranges = list(epsilon = seq(0.2,0.6,0.01), cost = 2^(2:6)))
print(model_svm)
plot(model_svm)

summary(model_svm)
model_P1 <- model_svm$best.model

results_svm <- predict(model_P1, val)

results_svm_ref <- as.data.frame(cbind(val, results_svm))

error <- results_svm_ref$QTY_ORDER - results_svm
RMSE <- rmse(error)
RMSE

# r2 validation sample

rdois <- lm(QTY_ORDER ~ results_svm, data = results_svm_ref)
summary(rdois)$r.squared
#r^2 = 0.60

plot(results_svm_ref$QTY_ORDER, results_svm_ref$results_svm)
abline(0,1, col = "red")
abline(rdois, col = "blue")

ggplot(results_svm_ref, aes(x = Price, y = QTY_ORDER))+
  geom_point(size = 4)+
  geom_point(aes(y = results_svm_ref$results_svm), col = "red")

