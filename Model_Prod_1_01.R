setwd("C:/Users/Marcelo/Desktop/Data Science/B2W Labs/B2WLabs/")

rm(list = ls())

# B2W LABs | Pricing Challenge

# Deliverables:
#  1) Models for Demand Forecasting: The main objective is to create a model to predict the quantity sold for
# each product given a prescribed price. Along with the statistical model, we need metrics, relationships and
# descriptions of these data in order to understand the sales behavior. What does the data tell us? How are
# the different data sources related? Is there a particular competitor that seems more important?
#

library(ggplot2)
P1_sales_by_weekday_month <- read.csv("P1_sales_by_weekday_month.csv", header = TRUE, stringsAsFactors = FALSE)

P1_sales_by_weekday_month$PROD_ID <- NULL

ggplot(P1_sales_by_weekday_month, aes(x = 1:63, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 1")

plot(P1_sales_by_weekday_month$Price, P1_sales_by_weekday_month$QTY_ORDER)

regP1 <- lm(QTY_ORDER ~ Price, data = P1_sales_by_weekday_month)
summary(regP1)
# r^2 = 0.40

#hist(regP1$residuals,prob = TRUE)
#lines(density(regP1$residuals), col = "red")

plot(P1_sales_by_weekday_month$Price, P1_sales_by_weekday_month$QTY_ORDER)
abline(regP1, col = "blue")

#********************* ANN Model************************************************
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
my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P1.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                tuneGrid = my.grid, trace = F, linout = 1) 

model <- P1.fit$bestTune
model

predicted <- predict(P1.fit, newdata = val_nn)
prestige.rmse <- sqrt(mean((predicted - val_resp)^2))
prestige.rmse
results <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot(results, aes(x = Price, y = val_resp))+
  geom_point(size = 4)+
  geom_point(aes(y = predicted), col = "red")

# r2 validation sample

rdois <- lm(val_resp ~ predicted, data = results)
summary(rdois)$r.squared
#r^2 = 0.507

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
val_nn$QTY_ORDER <- val_resp
val_nn_temp <- val_nn$Price
val_nn$Price <- NULL
val_nn$Price <- val_nn_temp
vall_rescaled <- val_nn* rep(b, each = nrow(val_nn)) + rep (a, each = nrow(val_nn))

results_sc <- cbind(val_nn[1:2], results$predicted, val_nn[4])

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(vall_rescaled, results_sc[3]))
names(results_scaled) <- c("month", "day", "QTY_ORDER", "Price", "ANN_QTY") 

RMSE_SC <- sqrt(mean(results_scaled$QTY_ORDER - results_scaled$ANN_QTY)^2)
RMSE_SC

plot(results_scaled$QTY_ORDER ~ results_scaled$ANN_QTY)
abline(0,1, col = "red")

# r2 validation sample

rdois <- lm(results_scaled$QTY_ORDER ~ results_scaled$ANN_QTY, data = results_scaled)
summary(rdois)$r.squared

## Predicting all the data set
train_temp_qty <- train_nn$QTY_ORDER
train_nn$QTY_ORDER <- NULL
pred_all <- predict(P1.fit, newdata = train_nn)

train_temp_price <- train_nn$Price
train_nn$Price <- NULL
train_nn$QTY_ORDER <- train_temp_qty
train_nn$Price <- train_temp_price

train_rescaled <- train_nn* rep(b, each = nrow(train_nn)) + rep (a, each = nrow(train_nn))

full_results_sc <- cbind(train_nn[1:2], pred_all, train_nn[4])

full_results_sc <-  full_results_sc*rep(b, each = nrow(full_results_sc)) + rep (a, each = nrow(full_results_sc))

full_data <- as.data.frame(cbind(train_rescaled, full_results_sc[3]))
names(full_data) <- c("month", "day", "QTY_ORDER", "Price", "ANN_QTY") 
full_data <- rbind(full_data, results_scaled)

ggplot(full_data, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = ANN_QTY), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P1")

rdois_full <- lm(full_data$QTY_ORDER ~ full_data$ANN_QTY, data = full_data)
summary(rdois_full)$r.squared
# r^2 = 0.53

#*******************************************************************************
# SVM Model
library(e1071)
# Split fulldata into train and test
set.seed(12345)
split = sample.split(data_P2$Price, SplitRatio = 0.75)
train <- subset(data_P2, split == TRUE)
val <- subset(data_P2, split == FALSE)

model_svm <- svm(f, data = train, scale = TRUE, kernel = 'radial',
                     cachesize = 1600, cross = 5, epsilon = 0.1)

svm_val <- predict(model_svm, val)

results_svm <- as.data.frame(cbind(val, svm_val))

RMSE_SVM <-  sqrt(mean(results_svm$QTY_ORDER - svm_val)^2)
RMSE_SVM

summary(model_svm)

# r2 validation sample

rdois <- lm(QTY_ORDER ~ svm_val, data = results_svm)
summary(rdois)$r.squared
#r^2 = 0.6329

ggplot(results_svm, aes(x = Price, y = QTY_ORDER))+
  geom_point(size = 4)+
  geom_point(aes(y = results_svm$svm_val), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P1")

svm_full <- predict(model_svm, data_P2)

full_results_svm <- as.data.frame(cbind(data_P2, svm_full))

RMSE_SVM <-  sqrt(mean(full_results_svm$QTY_ORDER - svm_full)^2)
RMSE_SVM

# r2 validation sample
rdois_2 <- lm(QTY_ORDER ~ svm_full, data = full_results_svm)
summary(rdois_2)$r.squared
#r^2 = 0.65

ggplot(full_results_svm, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = full_results_svm$svm_full), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P1")

ggplot()+
  geom_point(data = full_results_svm, aes(x = 1:63, y = QTY_ORDER, 
                                        size = Price, group = day, color = factor(month)))+
  geom_line(data = full_results_svm, aes(x = 1:63, y = QTY_ORDER), col = "blue")+
  geom_point(data = full_results_svm, aes(x = 1:63, y = svm_full, size = Price), pch = 4, col = "red")+
  xlab("Weekdays per Month")+
  ylab("Weekdays Sales Product 1")+
  ggtitle("Weekday Sales Product 1 - SVM Model")+
  theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  annotate("text", x = 5, y = 150, label = "R^2 = 0.65")
