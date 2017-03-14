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
P9_sales_by_weekday_month <- read.csv("P9_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P9_sales_by_weekday_month$PROD_ID <- NULL
P9_sales_by_weekday_month$REVENUE <- NULL
P9_sales <- P9_sales_by_weekday_month
P9_sales <- P9_sales[,c(1,2,4,3)] #  QTY_ORDER in the last column

ggplot(P9_sales, aes(x = 1:56, y = QTY_ORDER))+
        geom_point(data = P9_sales, aes(group = day, color = factor(month)), size = 3)+
        geom_line(data = P9_sales, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 9")


plot(P9_sales$Price, log(P9_sales$QTY_ORDER))
# Removing outliers
P9_sales <- subset(P9_sales, log(QTY_ORDER) > 2)

plot(P9_sales$Price, log(P9_sales$QTY_ORDER))

regP9 <- glm(log(QTY_ORDER) ~ Price + month, family = gaussian, data = P9_sales)
summary(regP9)
mean(regP9$residuals)
hist(regP9$residuals)
fitP9 <- predict(regP9, newdata = P9_sales, type = "response")
plot(P9_sales$Price, log(P9_sales$QTY_ORDER))
points(P9_sales$Price, fitP9, col = "red")

rdois_glm <- lm(log(P9_sales$QTY_ORDER) ~ fitP9)
summary(rdois_glm)$r.squared
#R^2 = 0.847

ggplot()+
  geom_point(data = P9_sales, aes(x = Price, y = log(QTY_ORDER), color = "black"), size = 4)+
  geom_point(aes(x = P9_sales$Price,  y = fitP9, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P9")+ 
  scale_colour_manual(name = " Quantidades P9", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

P9_sales_GLM <- P9_sales
P9_sales_GLM$GLM <- exp(fitP9)


ggplot()+
  geom_point(data = P9_sales_GLM, aes(x = 1:55, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = P9_sales_GLM, aes(x = 1:55, y = GLM), size = 2, color = "red")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 9")

names(P9_sales)

feats <- names(P9_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

P9_sales$QTY_ORDER <- log(P9_sales$QTY_ORDER)

#*******************************************************************************
# MLP Model
library(nnet)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P9_sales$Price, SplitRatio = 0.80)
train_nn <- subset(P9_sales, split == TRUE)
val_nn <- subset(P9_sales, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P9.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p9 <- P9.fit$bestTune
model_p9

predicted <- predict(P9.fit, newdata = val_nn)
p9fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p9fit.rmse
results_P9.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P9.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P9.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P9")+ 
  scale_colour_manual(name = " Quantidades P9", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P9.fit <- lm(val_resp ~ predicted, data = results_P9.fit)
summary(rdois_P9.fit)$r.squared
mean(rdois_P9.fit$residuals)
#R^2 = 0.772.

# full dataset
predicted_full <- predict(P9.fit, newdata = P9_sales)
full_p9fit.rmse <- sqrt(mean((predicted_full - P9_sales$QTY_ORDER)^2))
full_p9fit.rmse

full_results_P9.fit <- as.data.frame(cbind(P9_sales, predicted_full))

ggplot()+
  geom_point(data = full_results_P9.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P9.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P9")+ 
  scale_colour_manual(name = " Quantidades P9", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P9.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P9.fit)
summary(full_rdois_P9.fit)$r.squared
mean(full_rdois_P9.fit$residuals)

full_results_P9.fit$QTY_ORDER <- exp(full_results_P9.fit$QTY_ORDER)
full_results_P9.fit$predicted_full <- exp(full_results_P9.fit$predicted_full)

ggplot()+
  geom_point(data = full_results_P9.fit,
             aes(x = 1:55, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = full_results_P9.fit, aes(x = 1:55, y = predicted_full), size = 3, color = "red", pch = 4)+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 9")
