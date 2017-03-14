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
P3_sales_by_weekday_month <- read.csv("P3_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P3_sales_by_weekday_month$PROD_ID <- NULL
P3_sales_by_weekday_month$REVENUE <- NULL

P3_sales <- P3_sales_by_weekday_month[,c(1,2,4,3)] #  QTY_ORDER in the last column

ggplot(P3_sales, aes(x = 1:63, y = QTY_ORDER))+
        geom_point(data = P3_sales, aes(group = day, color = factor(month)), size = 3)+
        geom_smooth()+
        geom_line(data = P3_sales, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 3")

plot(P3_sales$Price, P3_sales$QTY_ORDER)

regP3 <- glm(QTY_ORDER ~ ., family = gaussian, data = P3_sales)
summary(regP3)
mean(regP3$residuals)
hist(regP3$residuals)
fit <- predict(regP3, newdata = P3_sales, type = "response")
plot(P3_sales$Price, P3_sales$QTY_ORDER)
points(P3_sales$Price, fit, col = "red")

rdois_glm <- lm(P3_sales$QTY_ORDER ~ fit)
summary(rdois_glm)$r.squared
#R^2 = 0.5238382

# removing day
regP3_opt <- glm(QTY_ORDER ~ Price + month, family = gaussian, data = P3_sales)
summary(regP3_opt)
mean(regP3_opt$residuals)
hist(regP3_opt$residuals)
fit_opt <- predict(regP3_opt, newdata = P3_sales, type = "response")
plot(P3_sales$Price, P3_sales$QTY_ORDER)
points(P3_sales$Price, fit_opt, col = "red")

rdois_glm_opt <- lm(P3_sales$QTY_ORDER ~ fit_opt)
summary(rdois_glm_opt)$r.squared
# R^2 = 0.5210221

# normalizing data
names(P3_sales)

feats <- names(P3_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P3_sales$Price, SplitRatio = 0.80)
train_nn <- subset(P3_sales, split == TRUE)
val_nn <- subset(P3_sales, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = c(0.1, 0.05, 0.01), .size = c(3, 4, 5, 6, 7, 8, 9))
P3.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p3 <- P3.fit$bestTune
model_p3

predicted <- predict(P3.fit, newdata = val_nn)
p3fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p3fit.rmse
results_P3.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P3.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P3.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P3")+ 
  scale_colour_manual(name = " Quantidades P3", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P3.fit <- lm(val_resp ~ predicted, data = results_P3.fit)
summary(rdois_P3.fit)$r.squared
mean(rdois_P3.fit$residuals)
#R^2 = 0.772.

# full dataset
predicted_full <- predict(P3.fit, newdata = P3_sales)
full_p3fit.rmse <- sqrt(mean((predicted_full - P3_sales$QTY_ORDER)^2))
full_p3fit.rmse

full_results_P3.fit <- as.data.frame(cbind(P3_sales, predicted_full))

ggplot()+
  geom_point(data = full_results_P3.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P3.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P3")+ 
  scale_colour_manual(name = " Quantidades P3", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P3.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P3.fit)
summary(full_rdois_P3.fit)$r.squared
mean(full_rdois_P3.fit$residuals)


