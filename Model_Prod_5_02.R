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
library(caret)
library(dplyr)
library(lubridate)

P5_sales_by_week_day_month <- read.csv("P5_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)
# Exploratory Analysis
# Por Dia
P5_by_DATE <- P5_sales_by_week_day_month %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot()+
        geom_point(data = P5_by_DATE, aes(x = 1:183, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 5")+
        ggtitle("Daily Sales Product 5")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P8_by_DATE)

# Por Mês
P5_by_month <- P5_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P5_by_month, aes(x = month, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P5_by_month, aes(x = month, y = Quantity), color = "blue")+
        xlab("Months")+
        ylab("Monthly Sales Product 5")+
        ggtitle("Monthly Sales Product 5")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P5_by_week <- P5_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P5_by_week, aes(x = 1:33, y = Quantity, color = Price,
                                          size = Price))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 5")+
        ggtitle("Weekly Sales Product 5")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por dia da semana
P5_by_weekday <- P5_sales_by_week_day_month %>%
        group_by(month, day) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P5_by_weekday, aes(x = 1:49, y = Quantity, color = factor(month),
                                             size = Price, group = day))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 5")+
        ggtitle("Weekly Sales Product 5")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Correlation Quantidade x Preço
cor(P5_by_DATE$QTY_ORDER, P5_by_DATE$Price) # -0.61
cor(P5_by_month$Quantity, P5_by_month$Price) # -0.82
cor(P5_by_week$Quantity, P5_by_week$Price) # -0.68

# Modeling
plot(P5_by_DATE$Price, P5_by_DATE$QTY_ORDER) #days
P5_sales <- P5_by_DATE
# # observations > 1000
P5_sales <- subset(P5_by_DATE, P5_by_DATE$QTY_ORDER < 1000)
plot(P5_sales$Price, P5_sales$QTY_ORDER) #days

P5_sales$DATE_ORDER <- ymd(P5_sales$DATE_ORDER)
P5_sales$DATE_ORDER <- as.numeric(P5_sales$DATE_ORDER)


regP5 <- nls(QTY_ORDER ~ exp(a+b*Price), data = P5_sales, start = list(a = 0, b = 0))
summary(regP5)
fit_regP5 <- predict(regP5, newdata = P5_sales, type = "response")
plot(P5_sales$Price, P5_sales$QTY_ORDER)
points(P5_sales$Price, fit_regP5, col = "red")

rdois_exp_opt <- lm(P5_sales$QTY_ORDER ~ fit_regP5)
summary(rdois_exp_opt)$r.squared
mean(rdois_exp_opt$residuals)
# r^2 = 0.8446.


P5_sales$PROD_ID <- NULL
P5_sales$REVENUE <- NULL
P5_sales <- P5_sales[,c(1, 3, 2)]

feats <- names(P5_sales)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P5_sales, 2, max)
mins <- apply(P5_sales, 2, min)
P5_scaled <- as.data.frame(scale(P5_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P5_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P5_scaled, split == TRUE)
val_nn <- subset(P5_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.4, 0.02), .size = seq(3, 10, 1))
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
#R^2 = 0.96.

# full dataset
predicted_full <- predict(P5.fit, newdata = P5_scaled)
full_p5fit.rmse <- sqrt(mean((predicted_full - P5_scaled$QTY_ORDER)^2))
full_p5fit.rmse

full_results_P5.fit <- as.data.frame(cbind(P5_scaled, predicted_full))

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

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P5_scaled[1:2], full_results_P5.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P5_sales$QTY_ORDER))
names(results_scaled) <- c("Date", "Price", "MLP", "Quantity")

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:180, y = Quantity, color = Price,
                                              size = Price))+
        geom_line(data = results_scaled, aes(x = 1:180, y = Quantity), col = "blue")+
        geom_point(data = results_scaled, aes(x = 1:180, y = MLP, size = Price), pch = 4, col = "red")+
        xlab("Days")+
        ylab("Daily Sales Product 5")+
        ggtitle("Weekly Sales Product 5 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 10, y = 800, label = "R^2 = 0.89")

#*********************************
#SVM Model
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P5_sales$Price, SplitRatio = 0.80)
train_svm <- subset(P5_sales, split == TRUE)
val_svm <- subset(P5_sales, split == FALSE)
val_resp_svm <- val_svm$QTY_ORDER # saving test results
val_svm$QTY_ORDER <- NULL

library(e1071)

tuned_model_svm <- tune(svm, QTY_ORDER ~ .,
                        data = train_svm, scale = TRUE, kernel = 'radial',
                        cachesize = 2000, ranges = list(gamma = 10^(-5:-1),
                                                        epsilon = seq(0.1,0.4,0.01),
                                                        cost = 2^(-6:9)))

print(tuned_model_svm)

tuned_model_P5 <- tuned_model_svm$best.model
summary(tuned_model_P5)

tuned_svm_val <- predict(tuned_model_P5, val_svm)

results_svm <- cbind.data.frame(val_svm, val_resp_svm, tuned_svm_val)

RMSE_tuned_SVM <-  sqrt(mean(val_resp_svm - tuned_svm_val)^2)
RMSE_tuned_SVM

# r2 validation sample

rdois_svm_te <- lm(val_resp_svm ~ tuned_svm_val)
summary(rdois_svm_te)$r.squared

# Plot Test data set
ggplot()+
        geom_point(data = results_svm, aes(x = Price, y = val_resp_svm, color = "black"), size = 4)+
        geom_point(data = results_svm, aes(x = Price, y = tuned_svm_val, color = "red"))+
        xlab("Preço")+
        ylab("Quantidade P5")+ 
        scale_colour_manual(name = " Quantidades P5", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# Full data set

tuned_svm_full <- predict(tuned_model_P5, P5_sales)
full_results_svm <- cbind.data.frame(P5_sales, tuned_svm_full)

RMSE_tuned_SVM <-  sqrt(mean(full_results_svm$QTY_ORDER - tuned_svm_full)^2)
RMSE_tuned_SVM

# r2 Full sample
rdois_svm_full <- lm(full_results_svm$QTY_ORDER ~ full_results_svm$tuned_svm_full)
summary(rdois_svm_full)$r.squared
mean(rdois_svm_full$residuals)

names(full_results_svm) <- c("Date", "Price", "SVM", "Quantity")

ggplot()+
        geom_point(data = full_results_svm, aes(x = 1:180, y = Quantity, color = Price,
                                              size = Price))+
        geom_line(data = full_results_svm, aes(x = 1:180, y = Quantity), col = "blue")+
        geom_point(data = full_results_svm, aes(x = 1:180, y = SVM), pch = 6, col = "orange")+
        geom_point(data = results_scaled, aes(x = 1:180, y = MLP), pch = 4, col = "red")+
        xlab("Days")+
        ylab("Daily Sales Product 5")+
        ggtitle("Weekly Sales Product 5 - MLP - SVM Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 30, y = 800, label = "R^2 MLP 'red' = 0.89")+
        annotate("text", x = 30, y = 600, label = "R^2 SVM 'orange' = 0.88")
