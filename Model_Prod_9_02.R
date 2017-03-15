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

P9_sales_by_week_day_month <- read.csv("P9_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)
# Exploratory Analysis
# Por Dia
P9_by_DATE <- P9_sales_by_week_day_month %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot()+
        geom_point(data = P9_by_DATE, aes(x = 1:210, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 9")+
        ggtitle("Daily Sales Product 9")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P8_by_DATE)

# Por Mês
P9_by_month <- P9_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P9_by_month, aes(x = month, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P9_by_month, aes(x = month, y = Quantity), color = "blue")+
        xlab("Months")+
        ylab("Monthly Sales Product 9")+
        ggtitle("Monthly Sales Product 9")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P9_by_week <- P9_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P9_by_week, aes(x = 1:36, y = Quantity, color = Price,
                                          size = Price))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 6")+
        ggtitle("Weekly Sales Product 6")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por dia da semana
P9_by_weekday <- P9_sales_by_week_day_month %>%
        group_by(month, day) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P9_by_weekday, aes(x = 1:56, y = Quantity, color = factor(month),
                                             size = Price, group = day))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 9")+
        ggtitle("Weekly Sales Product 9")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Correlation Quantidade x Preço
cor(P9_by_DATE$QTY_ORDER, P9_by_DATE$Price) # -0.58
cor(P9_by_month$Quantity, P9_by_month$Price) # -0.79
cor(P9_by_week$Quantity, P9_by_week$Price) # -0.67

# Modeling
plot(P9_by_DATE$Price, log(P9_by_DATE$QTY_ORDER)) #days
P9_sales <- P9_by_DATE

P9_sales$DATE_ORDER <- ymd(P9_sales$DATE_ORDER)
P9_sales$DATE_ORDER <- as.numeric(P9_sales$DATE_ORDER)


regP9 <- glm(log(QTY_ORDER) ~ Price + DATE_ORDER, data = P9_sales)
summary(regP9)
fit_regP9 <- predict(regP9, newdata = P9_sales, type = "response")
plot(P9_sales$Price, log(P9_sales$QTY_ORDER), 
     main = "P9 Sales X Price - GLM Model",
     xlab = "Price", ylab = "log(QTY ORDER)")
points(P9_sales$Price, fit_regP9, col = "red")
legend("topright", inset=.05, c("black = Obs.","red = Predicted"))

rdois_exp_opt <- lm(log(P9_sales$QTY_ORDER) ~ fit_regP9)
summary(rdois_exp_opt)$r.squared
mean(rdois_exp_opt$residuals)
# r^2 = 0.6847.


P9_sales$PROD_ID <- NULL
P9_sales$REVENUE <- NULL
P9_sales <- P9_sales[,c(1, 3, 2)]
P9_sales$QTY_ORDER <- log(P9_sales$QTY_ORDER)

feats <- names(P9_sales)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P9_sales, 2, max)
mins <- apply(P9_sales, 2, min)
P9_scaled <- as.data.frame(scale(P9_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P9_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P9_scaled, split == TRUE)
val_nn <- subset(P9_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.4, 0.02), .size = seq(3, 10, 1))
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
        ylab("Quantidade P5")+ 
        scale_colour_manual(name = " Quantidades P9", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P9.fit <- lm(val_resp ~ predicted, data = results_P9.fit)
summary(rdois_P9.fit)$r.squared
mean(rdois_P9.fit$residuals)
#R^2 = 0.84.

# full dataset
predicted_full <- predict(P9.fit, newdata = P9_scaled)
full_p9fit.rmse <- sqrt(mean((predicted_full - P9_scaled$QTY_ORDER)^2))
full_p9fit.rmse

full_results_P9.fit <- as.data.frame(cbind(P9_scaled, predicted_full))

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

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P9_scaled[1:2], full_results_P9.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P9_sales$QTY_ORDER))
names(results_scaled) <- c("Date", "Price", "MLP", "Quantity")

# Backtransforming QTY-ORDER
results_scaled$Quantity <- exp(results_scaled$Quantity)
results_scaled$MLP <- exp(results_scaled$MLP)

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:210, y = Quantity, color = Price,
                                              size = Price))+
        geom_line(data = results_scaled, aes(x = 1:210, y = Quantity), col = "blue")+
        geom_point(data = results_scaled, aes(x = 1:210, y = MLP, size = Price), pch = 4, col = "red")+
        xlab("Days")+
        ylab("Daily Sales Product 9")+
        ggtitle("Weekly Sales Product 9 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 10, y = 800, label = "R^2 = 0.81")

