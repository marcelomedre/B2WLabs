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
P4_sales_by_weekday_month <- read.csv("P4_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P4_sales_by_weekday_month$PROD_ID <- NULL
P4_sales_by_weekday_month$REVENUE <- NULL

P4_sales <- P4_sales_by_weekday_month[,c(1,2,4,3)] #  QTY_ORDER in the last column

ggplot(P4_sales, aes(x = 1:51, y = QTY_ORDER))+
        geom_point(data = P4_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
        geom_smooth()+
        geom_line(data = P4_sales_by_weekday_month, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 4")


plot(P4_sales$Price, log(P4_sales$QTY_ORDER))

regP4 <- glm(log(QTY_ORDER) ~ ., family = gaussian, data = P4_sales)
summary(regP4)
mean(regP4$residuals)
hist(regP4$residuals)
fit <- predict(regP4, newdata = P4_sales, type = "response")
plot(P4_sales$Price, P4_sales$QTY_ORDER)
points(P4_sales$Price, exp(fit), col = "red")

rdois_glm <- lm(log(P4_sales$QTY_ORDER) ~ fit)
summary(rdois_glm)$r.squared
#R^2 = 0.618

ggplot()+
  geom_point(data = P4_sales, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(aes(x = P4_sales$Price,  y = exp(fit), color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P4")+ 
  scale_colour_manual(name = " Quantidades P4", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

P4_sales_GLM <- P4_sales
P4_sales_GLM$GLM <- exp(fit)

ggplot()+
  geom_point(data = P4_sales_GLM, aes(x = 1:51, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = P4_sales_GLM, aes(x = 1:51, y = GLM), size = 2, color = "red")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 4")

P4_sales$QTY_ORDER <- log(P4_sales$QTY_ORDER)

names(P4_sales)

feats <- names(P4_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P4_sales$Price, SplitRatio = 0.80)
train_nn <- subset(P4_sales, split == TRUE)
val_nn <- subset(P4_sales, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P4.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p4 <- P4.fit$bestTune
model_p4

predicted <- predict(P4.fit, newdata = val_nn)
p4fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p4fit.rmse
results_P4.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P4.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P4.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P4")+ 
  scale_colour_manual(name = " Quantidades P4", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P4.fit <- lm(val_resp ~ predicted, data = results_P4.fit)
summary(rdois_P4.fit)$r.squared
mean(rdois_P4.fit$residuals)
#R^2 = 0.772.

# full dataset
predicted_full <- predict(P4.fit, newdata = P4_sales)
full_p4fit.rmse <- sqrt(mean((predicted_full - P4_sales$QTY_ORDER)^2))
full_p4fit.rmse

full_results_P4.fit <- as.data.frame(cbind(P4_sales, predicted_full))

ggplot()+
  geom_point(data = full_results_P4.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P4.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P4")+ 
  scale_colour_manual(name = " Quantidades P4", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P4.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P4.fit)
summary(full_rdois_P4.fit)$r.squared
mean(full_rdois_P4.fit$residuals)

full_results_P4.fit$QTY_ORDER <- exp(full_results_P4.fit$QTY_ORDER)
full_results_P4.fit$predicted_full <- exp(full_results_P4.fit$predicted_full)

ggplot()+
  geom_point(data = full_results_P4.fit,
             aes(x = 1:51, y = QTY_ORDER, group = day, color = factor(month)), size = 3)+
  geom_point(data = full_results_P4.fit, aes(x = 1:51, y = predicted_full), size = 3, color = "red")+
  geom_point(data = P4_sales_GLM, aes(x = 1:51, y = GLM), size = 3, color = "black")+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 4")
