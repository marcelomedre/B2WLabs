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
P7_sales_by_week_day_month <- read.csv("P7_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)
# Exploratory Analysis
# Por Dia
P7_by_DATE <- P7_sales_by_week_day_month %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

mean(P7_by_DATE$QTY_ORDER)
sd(P7_by_DATE$QTY_ORDER)
sum(P7_by_DATE$QTY_ORDER > 1500) # 29 dias 
cv = (sd(P7_by_DATE$QTY_ORDER)/mean(P7_by_DATE$QTY_ORDER)) # coef variação

ggplot()+
        geom_point(data = P7_by_DATE, aes(x = 1:285, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 7")+
        ggtitle("Daily Sales Product 7")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))
summary(P7_by_DATE)

# Por Mês
P7_by_month <- P7_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P7_by_month, aes(x = 1:10, y = Quantity, color = Price,
                                           size = Price))+
        xlab("Months")+
        ylab("Monthly Sales Product 7")+
        ggtitle("Monthly Sales Product 7")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P7_by_week <- P7_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P7_by_week, aes(x = 1:42, y = Quantity, color = Price,
                                          size = Price))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 7")+
        ggtitle("Weekly Sales Product 7")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Correlation Quantidade x Preço
cor(P7_by_DATE$QTY_ORDER, P7_by_DATE$Price) # -0.38
cor(P7_by_month$Quantity, P7_by_month$Price) # 0.008
cor(P7_by_week$Quantity, P7_by_week$Price) # -0.23

# Modeling
plot(P7_by_week$Price, P7_by_week$Quantity)

Price <- P7_by_week$Price
Quantity <- P7_by_week$Quantity

names(P7_by_week)
P7_sales <- P7_by_week[,c(1, 2, 8, 3)]

feats <- names(P7_sales)[-c(1,4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("Quantity ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P7_sales, 2, max)
mins <- apply(P7_sales, 2, min)
P2_scaled <- as.data.frame(scale(P7_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P2_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P2_scaled, split == TRUE)
val_nn <- subset(P2_scaled, split == FALSE)
val_resp <- val_nn$Quantity # saving test results
val_nn$Quantity <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.2, 0.02), .size = seq(3, 15, 1))
P7.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p7 <- P7.fit$bestTune
model_p7

predicted <- predict(P7.fit, newdata = val_nn)
p7fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p7fit.rmse
results_P7.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P7.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P7.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P5")+ 
  scale_colour_manual(name = " Quantidades P5", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P7.fit <- lm(val_resp ~ predicted, data = results_P7.fit)
summary(rdois_P7.fit)$r.squared
mean(rdois_P7.fit$residuals)
#R^2 = 0.167.

# full dataset
predicted_full <- predict(P7.fit, newdata = P2_scaled)
full_p7fit.rmse <- sqrt(mean((predicted_full - P2_scaled$Quantity)^2))
full_p7fit.rmse

full_results_P7.fit <- as.data.frame(cbind(P2_scaled, predicted_full))

ggplot()+
  geom_point(data = full_results_P7.fit, aes(x = Price, y = Quantity, color = "black"), size = 4)+
  geom_point(data = full_results_P7.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P6")+ 
  scale_colour_manual(name = " Quantidades P6", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P7.fit <- lm(Quantity ~ predicted_full, data = full_results_P7.fit)
summary(full_rdois_P7.fit)$r.squared
mean(full_rdois_P7.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
val_nn_temp <- val_nn$Price
val_nn$Price <- NULL
val_nn$Quantity <- val_resp
val_nn$Price <- val_nn_temp
vall_rescaled <- val_nn* rep(b, each = nrow(val_nn)) + rep (a, each = nrow(val_nn))

results_sc <- cbind(P2_scaled[1:3], full_results_P7.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P7_sales$Quantity))
names(results_scaled) <- c("month", "week", "Price", "MLP", "Quantity")

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:49, y = Quantity, color = Price,
                                          size = Price))+
        geom_point(data = results_scaled, aes(x = 1:49, y = MLP, size = Price), pch = 4)+
        xlab("Weeks")+
        ylab("Weekly Sales Product 7")+
        ggtitle("Weekly Sales Product 7 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 4, y = 10000, label = "R^2 = 0.46")


#*********************************
#SVM Model
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P7_sales$Price, SplitRatio = 0.80)
train_svm <- subset(P7_sales, split == TRUE)
val_svm <- subset(P7_sales, split == FALSE)
val_resp_svm <- val_svm$Quantity # saving test results
val_svm$Quantity <- NULL

library(e1071)

tuned_model_svm <- tune(svm, Quantity ~ Price + week + month,
                        data = train_svm, scale = TRUE, kernel = 'radial',
                        cachesize = 2000, ranges = list(gamma = 10^(-5:-1),
                                                        epsilon = seq(0.1,0.4,0.01),
                                                        cost = 10^(-3:2.5)))

print(tuned_model_svm)

tuned_model_P7 <- tuned_model_svm$best.model
summary(tuned_model_P7)

tuned_svm_val <- predict(tuned_model_P7, val_svm)

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

tuned_svm_full <- predict(tuned_model_P7, P7_sales)
full_results_svm <- cbind.data.frame(P7_sales, tuned_svm_full)

RMSE_tuned_SVM <-  sqrt(mean(full_results_svm$Quantity - tuned_svm_full)^2)
RMSE_tuned_SVM

# r2 Full sample
rdois_svm_full <- lm(full_results_svm$Quantity ~ full_results_svm$tuned_svm_full)
summary(rdois_svm_full)$r.squared
mean(rdois_svm_full$residuals)

names(full_results_svm) <- c("month", "week", "Price", "Quantity", "SVM")

ggplot()+
        geom_point(data = full_results_svm, aes(x = 1:49, y = Quantity, color = Price,
                                              size = Price))+
        geom_point(data = full_results_svm, aes(x = 1:49, y = SVM, size = Price), pch = 4)+
        xlab("Weeks")+
        ylab("Weekly Sales Product 7")+
        ggtitle("Weekly Sales Product 7 - SVM Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 4, y = 10000, label = "R^2 = 0.43")
#*********************************************************************
tuned_model_svm <- tune(svm, Quantity ~ Price + week,
                        data = train_svm, scale = TRUE, kernel = 'radial',
                        cachesize = 2000, ranges = list(gamma = 10^(-5:-1),
                                                        epsilon = seq(0.1,0.4,0.01),
                                                        cost = 10^(-3:3)))

print(tuned_model_svm)

tuned_model_P7 <- tuned_model_svm$best.model
summary(tuned_model_P7)

tuned_svm_val <- predict(tuned_model_P7, val_svm)

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

tuned_svm_full <- predict(tuned_model_P7, P7_sales)
full_results_svm <- cbind.data.frame(P7_sales, tuned_svm_full)

RMSE_tuned_SVM <-  sqrt(mean(full_results_svm$Quantity - tuned_svm_full)^2)
RMSE_tuned_SVM

# r2 Full sample
rdois_svm_full <- lm(full_results_svm$Quantity ~ full_results_svm$tuned_svm_full)
summary(rdois_svm_full)$r.squared
mean(rdois_svm_full$residuals)

names(full_results_svm) <- c("month", "week", "Price", "Quantity", "SVM")

ggplot()+
        geom_point(data = full_results_svm, aes(x = 1:49, y = Quantity, color = Price,
                                                size = Price))+
        geom_point(data = full_results_svm, aes(x = 1:49, y = SVM, size = Price), pch = 4)+
        xlab("Weeks")+
        ylab("Weekly Sales Product 7")+
        ggtitle("Weekly Sales Product 7 - SVM Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 4, y = 10000, label = "R^2 = 0.43")

