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
library(caTools)
library(caret)
P5_sales_by_weekday_month <- read.csv("P5_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P5_sales_by_weekday_month$PROD_ID <- NULL
P5_sales_by_weekday_month$REVENUE <- NULL
P5_sales <- P5_sales_by_weekday_month
P5_sales <- P5_sales[,c(1,2,4,3)] #  QTY_ORDER in the last column

ggplot(P5_sales, aes(x = 1:49, y = QTY_ORDER))+
        geom_point(data = P5_sales, aes(group = day, color = factor(month)), size = 3)+
        geom_smooth()+
        geom_line(data = P5_sales, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 5")


plot(P5_sales$Price, log(P5_sales$QTY_ORDER))

regP5 <- lm(log(QTY_ORDER) ~ poly(Price, 4, raw = TRUE), data = P5_sales)
summary(regP5)
mean(regP5$residuals)
hist(regP5$residuals)
fitP5 <- predict(regP5, newdata = P5_sales, type = "response")
plot(P5_sales$Price, log(P5_sales$QTY_ORDER))
points(P5_sales$Price, fitP5, col = "red")


rdois_poly <- lm(log(P5_sales$QTY_ORDER) ~ fitP5)
summary(rdois_poly)$r.squared
#R^2 = 0.938

ggplot()+
  geom_point(data = P5_sales, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(aes(x = P5_sales$Price,  y = exp(fitP5), color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

P5_sales_poly <- P5_sales
P5_sales_poly$poly <- exp(exp(fitP5))


ggplot()+
  geom_point(data = P5_sales_poly, aes(x = 1:49, y = exp(QTY_ORDER), group = day, color = factor(month)), size = 3)+
  geom_point(data = P5_sales_poly, aes(x = 1:49, y = poly), size = 2, color = "red")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 5")

P5_sales$QTY_ORDER <- log(P5_sales$QTY_ORDER)

names(P5_sales)

feats <- names(P5_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P5_sales$Price, SplitRatio = 0.80)
train_nn <- subset(P5_sales, split == TRUE)
val_nn <- subset(P5_sales, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P5.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p5 <- P5.fit$bestTune
model_p5

predicted <- predict(P5.fit, newdata = val_nn)
p5fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p5fit.rmse
results_P5.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P5.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P5.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P5.fit <- lm(val_resp ~ predicted, data = results_P5.fit)
summary(rdois_P5.fit)$r.squared
mean(rdois_P5.fit$residuals)
#R^2 = 0.772.

# full dataset
predicted_full <- predict(P5.fit, newdata = P5_sales)
full_p5fit.rmse <- sqrt(mean((predicted_full - P5_sales$QTY_ORDER)^2))
full_p5fit.rmse

full_results_P5.fit <- as.data.frame(cbind(P5_sales, predicted_full))

ggplot()+
  geom_point(data = full_results_P5.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P5.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P5.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P5.fit)
summary(full_rdois_P5.fit)$r.squared
mean(full_rdois_P5.fit$residuals)

full_results_P5.fit$QTY_ORDER <- exp(full_results_P5.fit$QTY_ORDER)
full_results_P5.fit$predicted_full <- exp(full_results_P5.fit$predicted_full)

ggplot()+
  geom_point(data = full_results_P5.fit,
             aes(x = 1:49, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = full_results_P5.fit, aes(x = 1:49, y = predicted_full), size = 3, color = "red")+
  geom_point(data = P5_sales_poly, aes(x = 1:49, y = poly), size = 3, color = "black")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 5")

#*******************************************************************************
library(e1071)

model_svm <- svm(f, data = train_nn, scale = TRUE, kernel = 'radial',
                 cachesize = 1600, cross = 5, epsilon = 0.1)

svm_val <- predict(model_svm, val_nn)

p5svm.rmse <- sqrt(mean((svm_val - val_resp)^2))
p5svm.rmse

summary(model_svm)

# r2 validation sample
rdois_svm <- lm(QTY_ORDER ~ svm_val, data = results_svm)
summary(rdois_svm)$r.squared
#r^2 = 0.851

results_svm <- as.data.frame(cbind(val_nn, val_resp, svm_val))

ggplot(results_svm, aes(x = Price, y = val_resp))+
  geom_point(size = 4)+
  geom_point(aes(y = results_svm$svm_val), col = "red")+
  xlab("Preço")+
  ylab("Quantidade P5")

# Refining Model

tuned_model_svm <- tune(svm, f, data = train_nn, scale = TRUE, kernel = 'radial',
                        cachesize = 2000, ranges = list(gamma = 10^(-5:-1),
                        epsilon = seq(0.1,0.4,0.01),
                        cost = 10^(-3:2.5)))
print(tuned_model_svm)

tuned_model_P5 <- tuned_model_svm$best.model
summary(tuned_model_P5)

tuned_svm_val <- predict(tuned_model_P5, val_nn)

results_tuned_svm <- as.data.frame(cbind(val_nn, val_resp, tuned_svm_val))

RMSE_tuned_SVM <-  sqrt(mean(results_tuned_svm$val_resp - tuned_svm_val)^2)
RMSE_tuned_SVM

# r2 validation sample

rdois_tuned_svm_te <- lm(val_resp ~ tuned_svm_val, data = results_tuned_svm)
summary(rdois_tuned_svm_te)$r.squared

#r^2 = 0.8906
ggplot(results_tuned_svm, aes(x = Price, y = val_resp))+
  geom_point(size = 4)+
  geom_point(aes(y = results_tuned_svm$tuned_svm_val), col = "red")+
  xlab("Preço")+
  ylab("Quantidade P5")

tuned_svm_full <- predict(tuned_model_P5, P5_sales)

full_results_tuned_svm <- as.data.frame(cbind(P5_sales, tuned_svm_full))

RMSE_SVM <-  sqrt(mean(full_results_tuned_svm$QTY_ORDER - tuned_svm_full)^2)
RMSE_SVM

# r2 full data set
rdois_2_full <- lm(QTY_ORDER ~ tuned_svm_full, data = full_results_tuned_svm)
summary(rdois_2_full)$r.squared

full_results_tuned_svm$QTY_ORDER <- exp(full_results_tuned_svm$QTY_ORDER)
full_results_tuned_svm$tuned_svm_full <- exp(full_results_tuned_svm$tuned_svm_full)

ggplot(full_results_tuned_svm, aes(x = Price, y = QTY_ORDER))+
  geom_point(size = 4)+
  geom_point(aes(y = full_results_tuned_svm$tuned_svm_full), col = "red")+
  xlab("Preço")+
  ylab("Quantidade P5")


ggplot()+
  geom_point(data = full_results_tuned_svm,
             aes(x = 1:49, y = QTY_ORDER, group = day), color = "black", size = 3)+
  geom_point(data = full_results_tuned_svm, aes(x = 1:49, y = tuned_svm_full), size = 3, color = "red")+
  geom_point(data = P5_sales_poly, aes(x = 1:49, y = poly), size = 3, color = "green")+
  geom_point(data = full_results_P5.fit, aes(x = 1:49, y = predicted_full), size = 3, color = "orange")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 5")


