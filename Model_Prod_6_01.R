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
P6_sales_by_weekday_month <- read.csv("P6_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P6_sales_by_weekday_month$PROD_ID <- NULL
P6_sales_by_weekday_month$REVENUE <- NULL
P6_sales <- P6_sales_by_weekday_month
P6_sales <- P6_sales[,c(1,2,4,3)] #  QTY_ORDER in the last column

ggplot(P6_sales, aes(x = 1:70, y = QTY_ORDER))+
        geom_point(data = P6_sales, aes(group = day, color = factor(month)), size = 3)+
        geom_line(data = P6_sales, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 6")


plot(P6_sales$Price, P6_sales$QTY_ORDER)

regP6 <- glm(QTY_ORDER ~ Price + month, family = gaussian, data = P6_sales)
summary(regP6)
mean(regP6$residuals)
hist(regP6$residuals)
fitP6 <- predict(regP6, newdata = P6_sales, type = "response")
plot(P6_sales$Price, P6_sales$QTY_ORDER)
points(P6_sales$Price, fitP6, col = "red")


rdois_poly <- lm(P6_sales$QTY_ORDER ~ fitP6)
summary(rdois_poly)$r.squared
#R^2 = 0.470

ggplot()+
  geom_point(data = P6_sales, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(aes(x = P6_sales$Price,  y = fitP6, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

P6_sales_GLM <- P6_sales
P6_sales_GLM$GLM <- fitP6


ggplot()+
  geom_point(data = P6_sales_GLM, aes(x = 1:70, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = P6_sales_GLM, aes(x = 1:70, y = GLM), size = 2, color = "red")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 6")

names(P6_sales)

feats <- names(P6_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P6_sales$Price, SplitRatio = 0.80)
train_nn <- subset(P6_sales, split == TRUE)
val_nn <- subset(P6_sales, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P6.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p6 <- P6.fit$bestTune
model_p6

predicted <- predict(P6.fit, newdata = val_nn)
p6fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p6fit.rmse
results_P6.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P6.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P6.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P6.fit <- lm(val_resp ~ predicted, data = results_P6.fit)
summary(rdois_P6.fit)$r.squared
mean(rdois_P6.fit$residuals)
#R^2 = 0.772.

# full dataset
predicted_full <- predict(P6.fit, newdata = P6_sales)
full_p6fit.rmse <- sqrt(mean((predicted_full - P6_sales$QTY_ORDER)^2))
full_p6fit.rmse

full_results_P6.fit <- as.data.frame(cbind(P6_sales, predicted_full))

ggplot()+
  geom_point(data = full_results_P6.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P6.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P6")+ 
  scale_colour_manual(name = " Quantidades P6", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P6.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P6.fit)
summary(full_rdois_P6.fit)$r.squared
mean(full_rdois_P6.fit$residuals)

ggplot()+
  geom_point(data = full_results_P6.fit,
             aes(x = 1:70, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = full_results_P6.fit, aes(x = 1:70, y = predicted_full), size = 3, color = "red", pch = 4)+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 6")
