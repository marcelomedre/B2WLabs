setwd("C:/Users/Marcelo/Desktop/Data/B2WLabs/")

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
P2_sales_by_weekday_month <- read.csv("P2_sales_by_weekday_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P2_sales_by_weekday_month$PROD_ID <- NULL

ggplot(P2_sales_by_weekday_month, aes(x = 1:70, y = QTY_ORDER))+
        geom_point(data = P2_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
        geom_smooth()+
        geom_line(data = P2_sales_by_weekday_month, aes(group = month, color = factor(month)))+
        xlab("Dias da Semana ao longo dos meses")+
        ylab("Quantidade vendida Produto 2")

plot(P2_sales_by_weekday_month$Price, log(P2_sales_by_weekday_month$QTY_ORDER))
# Removing outliers
out <- P2_sales_by_weekday_month$Price < 4.5
P2_sales_out <- subset(P2_sales_by_weekday_month, log(QTY_ORDER) > 5 & log(QTY_ORDER) < 8.0)

plot(P2_sales_out$Price, P2_sales_out$QTY_ORDER)


regP2 <- lm(QTY_ORDER ~ Price, data = P2_sales_out)
summary(regP2)
# r^2 = 0.28
# Formula

names(P2_sales_out)

feats <- names(P2_sales_out)[-(3:4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# SVM Model
library(e1071)
# Split fulldata into train and test
set.seed(12345)
split = sample.split(P2_sales_out$Price, SplitRatio = 0.75)
train <- subset(P2_sales_out, split == TRUE)
val <- subset(P2_sales_out, split == FALSE)

model_svm <- svm(f, data = train, scale = TRUE, kernel = 'radial',
                 cachesize = 1600, cross = 5, epsilon = 0.1)

svm_val <- predict(model_svm, val)

results_svm <- as.data.frame(cbind(val, svm_val))

RMSE_SVM <-  sqrt(mean(results_svm$QTY_ORDER - svm_val)^2)
RMSE_SVM

summary(model_svm)

# r2 validation sample

rdois_svm <- lm(QTY_ORDER ~ svm_val, data = results_svm)
summary(rdois_svm)$r.squared
#r^2 = 0.851

ggplot(results_svm, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = results_svm$svm_val), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P2")

# Refining Model

svm_full <- predict(model_svm, P2_sales_out)

full_results_svm <- as.data.frame(cbind(P2_sales_out, svm_full))

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
        ylab("Quantidade P2")

# Refining Model
tuned_model_svm <- tune(svm, f, data = train, scale = TRUE, kernel = 'radial',
                  cachesize = 2000, ranges = list(epsilon = seq(0.01,0.4,0.01),
                                                  cost = 2^(2:9)))
print(tuned_model_svm)
plot(tuned_model_svm)

tuned_model_P2 <- tuned_model_svm$best.model
summary(tuned_model_P2)

tuned_svm_val <- predict(tuned_model_P2, val)

results_tuned_svm <- as.data.frame(cbind(val, tuned_svm_val))

RMSE_SVM <-  sqrt(mean(results_tuned_svm$QTY_ORDER - tuned_svm_val)^2)
RMSE_SVM

# r2 validation sample

rdois_tuned_svm_te <- lm(QTY_ORDER ~ tuned_svm_val, data = results_tuned_svm)
summary(rdois_tuned_svm_te)$r.squared
#r^2 = 0.787

ggplot(results_tuned_svm, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = results_tuned_svm$tuned_svm_val), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P2")

tuned_svm_full <- predict(tuned_model_P2, P2_sales_out)

full_results_tuned_svm <- as.data.frame(cbind(P2_sales_out, tuned_svm_full))

RMSE_SVM <-  sqrt(mean(full_results_tuned_svm$QTY_ORDER - tuned_svm_full)^2)
RMSE_SVM

# r2 validation sample
rdois_2_full <- lm(QTY_ORDER ~ tuned_svm_full, data = full_results_tuned_svm)
summary(rdois_2_full)$r.squared
#r^2 = 0.8411

ggplot(full_results_tuned_svm, aes(x = Price, y = QTY_ORDER))+
        geom_point(size = 4)+
        geom_point(aes(y = full_results_tuned_svm$tuned_svm_full), col = "red")+
        xlab("Preço")+
        ylab("Quantidade P2")


