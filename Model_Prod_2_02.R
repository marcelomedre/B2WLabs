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

P2_sales_by_week_day_month <- read.csv("P2_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P2_sales_by_week_day_month$PROD_ID <- NULL
# Exploratory Analysis
# Por Dia
P2_by_DATE <- P2_sales_by_week_day_month %>%
        group_by(DATE_ORDER) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

summary(P2_by_DATE)
# Removing outliers
P2_by_DATE <- subset(P2_by_DATE, QTY_ORDER < 2500)

ggplot()+
        geom_point(data = P2_by_DATE, aes(x = 1:285, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 2")+
        ggtitle("Daily Sales Product 2")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P2_by_DATE)
# Por Mês
P2_by_month <- P2_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P2_by_month, aes(x = 1:10, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P2_by_month, aes(x = 1:10, y = Quantity))+
        xlab("Month")+
        ylab("Monthly Sales Product 2")+
        ggtitle("Monthly Sales Product 2")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P2_by_week <- P2_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P2_by_week, aes(x = 1:49, y = Quantity, color = Price,
                                          size = Price))+
        geom_line(data = P2_by_week, aes(x = 1:49, y = Quantity))+
        xlab("Week")+
        ylab("Weekly Sales Product 2")+
        ggtitle("Weekly Sales Product 2")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Correlation Quantidade x Preço
cor(P2_by_DATE$QTY_ORDER, P2_by_DATE$Price) # -0.44
cor(P2_by_month$Quantity, P2_by_month$Price) # -0.38
cor(P2_by_week$Quantity, P2_by_week$Price) # -0.46

# Modeling

# Removing outliers
out <- P2_sales_by_weekday_month$Price < 4.5
P2_sales_out <- subset(P2_sales_by_weekday_month, log(QTY_ORDER) > 5 & log(QTY_ORDER) < 8.0)

plot(P2_by_DATE$Price, log(P2_by_DATE$QTY_ORDER)) # dia
plot(P2_by_week$Price, log(P2_by_week$Quantity)) # semana
plot(P2_by_month$Price, P2_by_month$Quantity) # mes

P2_by_week <- subset(P2_by_week, log(Quantity) > 5)

regP2 <- glm(log(Quantity) ~ Price + week, data = P2_by_week)
summary(regP2)
fit_regP2 <- predict(regP2, newdata = P2_by_week, type = "response")
plot(P2_by_week$Price, log(P2_by_week$Quantity))
points(P2_by_week$Price, fit_regP2, col = "red")

rdois_glm_opt <- lm(log(P2_by_week$Quantity) ~ fit_regP2)
summary(rdois_glm_opt)$r.squared
mean(rdois_glm_opt$residuals)
# r^2 = 0.7209

# Formula
P2_sales <- P2_by_week[,c(1, 2, 8, 3)]
# COnverting Quantity to log
P2_sales$Quantity <- log(P2_sales$Quantity)

feats <- names(P2_sales)[-c(1,4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("Quantity ~", f))
f
#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P2_sales, 2, max)
mins <- apply(P2_sales, 2, min)
P2_scaled <- as.data.frame(scale(P2_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P2_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P2_scaled, split == TRUE)
val_nn <- subset(P2_scaled, split == FALSE)
val_resp <- val_nn$Quantity # saving test results
val_nn$Quantity <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.3, 0.05), .size = seq(3, 15, 1))
P2.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p2 <- P2.fit$bestTune
model_p2

predicted <- predict(P2.fit, newdata = val_nn)
p2fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p2fit.rmse
results_P2.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
        geom_point(data = results_P2.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
        geom_point(data = results_P2.fit, aes(x = Price, y = predicted, color = "red"))+
        xlab("Preço")+
        ylab("Quantidade P5")+ 
        scale_colour_manual(name = " Quantidades P5", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P2.fit <- lm(val_resp ~ predicted, data = results_P2.fit)
summary(rdois_P2.fit)$r.squared
mean(rdois_P2.fit$residuals)
#R^2 = 0.80

# full dataset
predicted_full <- predict(P2.fit, newdata = P2_scaled)
full_p2fit.rmse <- sqrt(mean((predicted_full - P2_scaled$Quantity)^2))
full_p2fit.rmse

full_results_P2.fit <- as.data.frame(cbind(P2_scaled, predicted_full))

ggplot()+
        geom_point(data = full_results_P2.fit, aes(x = Price, y = Quantity, color = "black"), size = 4)+
        geom_point(data = full_results_P2.fit, aes(x = Price, y = predicted_full, color = "red"))+
        xlab("Preço")+
        ylab("Quantidade P2")+ 
        scale_colour_manual(name = " Quantidades P2", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P2.fit <- lm(Quantity ~ predicted_full, data = full_results_P2.fit)
summary(full_rdois_P2.fit)$r.squared
mean(full_rdois_P2.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
val_nn$Quantity <- val_resp
vall_rescaled <- val_nn* rep(b, each = nrow(val_nn)) + rep (a, each = nrow(val_nn))

results_sc <- cbind(P2_scaled[1:3], full_results_P2.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P2_sales$Quantity))
names(results_scaled) <- c("month", "week", "Price", "MLP", "Quantity")

#Back transforming log(quantity)
results_scaled$MLP <- exp(results_scaled$MLP)
results_scaled$Quantity <- exp(results_scaled$Quantity)

GLM_data <- cbind.data.frame(results_scaled$Price, fit_regP2)
names(GLM_data) <- c("Price", "GLM")
GLM_data$GLM <- exp(GLM_data$GLM)

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:47, y = Quantity, color = Price,
                                              size = Price))+
        geom_line(data = results_scaled, aes(x = 1:47, y = Quantity), color = "blue")+
        geom_point(data = results_scaled, aes(x = 1:47, y = MLP, size = Price), pch = 6, col = "red")+
        xlab("Weeks")+
        ylab("Weekly Sales Product 2")+
        ggtitle("Weekly Sales Product 2 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 4, y = 6500, label = "R^2 = 0.72")

#*******************************************************************************
#SVM Model
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P2_sales$Price, SplitRatio = 0.80)
train_svm <- subset(P2_sales, split == TRUE)
val_svm <- subset(P2_sales, split == FALSE)
val_resp_svm <- val_svm$Quantity # saving test results
val_svm$Quantity <- NULL

library(e1071)

tuned_model_svm <- tune(svm, Quantity ~ Price + week,
                        data = train_svm, scale = TRUE, kernel = 'radial',
                        cachesize = 2000, ranges = list(gamma = 10^(-5:-1),
                                                        epsilon = seq(0.1,0.4,0.01),
                                                        cost = 10^(-3:4)))

print(tuned_model_svm)

tuned_model_P2 <- tuned_model_svm$best.model
summary(tuned_model_P2)

tuned_svm_val <- predict(tuned_model_P2, val_svm)

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
        ylab("Quantidade P6")+ 
        scale_colour_manual(name = " Quantidades P6", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# Full data set

tuned_svm_full <- predict(tuned_model_P2, P2_sales)
full_results_svm <- cbind.data.frame(P2_sales, tuned_svm_full)

RMSE_tuned_SVM <-  sqrt(mean(full_results_svm$Quantity - tuned_svm_full)^2)
RMSE_tuned_SVM

# r2 Full sample
rdois_svm_full <- lm(full_results_svm$Quantity ~ full_results_svm$tuned_svm_full)
summary(rdois_svm_full)$r.squared
mean(rdois_svm_full$residuals)

names(full_results_svm) <- c("month", "week", "Price", "Quantity", "SVM")
full_results_svm$Quantity <- exp(full_results_svm$Quantity)
full_results_svm$SVM <- exp(full_results_svm$SVM)

ggplot()+
        geom_point(data = full_results_svm, aes(x = 1:47, y = Quantity, color = Price,
                                                size = Price))+
        geom_point(data = full_results_svm, aes(x = 1:47, y = SVM, size = Price), pch = 4)+
        geom_point(data = results_scaled, aes(x = 1:47, y = MLP, size = Price), pch = 6, col = "red")+
        xlab("Weeks")+
        ylab("Weekly Sales Product 7")+
        ggtitle("Weekly Sales Product 7 - SVM Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 4, y = 7000, label = "R^2 = 0.43")
