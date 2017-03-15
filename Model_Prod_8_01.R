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
P8_sales_by_week_day_month <- read.csv("P8_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)
# Exploratory Analysis
# Por Dia
P8_by_DATE <- P8_sales_by_week_day_month %>%
        group_by(DATE_ORDER, PROD_ID) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot()+
        geom_point(data = P8_by_DATE, aes(x = 1:210, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 8")+
        ggtitle("Daily Sales Product 8")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P8_by_DATE)

# Por Mês
P8_by_month <- P8_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P8_by_month, aes(x = 1:8, y = Quantity, color = Price,
                                           size = Price))+
        xlab("Months")+
        ylab("Monthly Sales Product 8")+
        ggtitle("Monthly Sales Product 8")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P8_by_week <- P8_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P8_by_week, aes(x = 1:36, y = Quantity, color = Price,
                                          size = Price))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 8")+
        ggtitle("Weekly Sales Product 8")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por dia da semana
P8_by_weekday <- P8_sales_by_week_day_month %>%
        group_by(month, day) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P8_by_weekday, aes(x = 1:56, y = Quantity, color = factor(month),
                                          size = Price, group = day))+
        xlab("Weeks")+
        ylab("Weekly Sales Product 8")+
        ggtitle("Weekly Sales Product 8")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Correlation Quantidade x Preço
cor(P8_by_DATE$QTY_ORDER, P8_by_DATE$Price) # -0.58
cor(P8_by_month$Quantity, P8_by_month$Price) # -0.87
cor(P8_by_week$Quantity, P8_by_week$Price) # -0.64

# Modeling
plot(P8_by_week$Price, log(P8_by_week$Quantity)) # weeks
plot(log(P8_by_DATE$Price), log(P8_by_DATE$QTY_ORDER)) #days

P8_sales <- P8_by_DATE
P8_sales$DATE_ORDER <- ymd(P8_sales$DATE_ORDER)
P8_sales$DATE_ORDER <- as.numeric(P8_sales$DATE_ORDER)

regP8 <- glm(log(QTY_ORDER) ~ log(Price) + DATE_ORDER, data = P8_sales)
summary(regP8)
fit_regP8 <- predict(regP8, newdata = P8_sales, type = "response")
plot(log(P8_by_DATE$Price), log(P8_by_DATE$QTY_ORDER))
points(log(P8_by_DATE$Price), fit_regP8, col = "red")

rdois_glm_opt <- lm(log(P8_by_DATE$QTY_ORDER) ~ fit_regP8)
summary(rdois_glm_opt)$r.squared
mean(rdois_glm_opt$residuals)
# r^2 = 0.7888

P8_sales$PROD_ID <- NULL
P8_sales$REVENUE <- NULL
P8_sales <- P8_sales[,c(1, 3, 2)]

# Converting QTY_ORder and Price to log
P8_sales$Price <- log(P8_sales$Price)
P8_sales$QTY_ORDER <- log(P8_sales$QTY_ORDER) 

feats <- names(P8_sales)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P8_sales, 2, max)
mins <- apply(P8_sales, 2, min)
P8_scaled <- as.data.frame(scale(P8_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P8_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P8_scaled, split == TRUE)
val_nn <- subset(P8_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.4, 0.02), .size = seq(3, 10, 1))
P8.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p8 <- P8.fit$bestTune
model_p8

predicted <- predict(P8.fit, newdata = val_nn)
p8fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p8fit.rmse
results_P8.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P8.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P8.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P8")+ 
  scale_colour_manual(name = " Quantidades P8", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P8.fit <- lm(val_resp ~ predicted, data = results_P8.fit)
summary(rdois_P8.fit)$r.squared
mean(rdois_P8.fit$residuals)
#R^2 = 0.82.

# full dataset
predicted_full <- predict(P8.fit, newdata = P8_scaled)
full_p8fit.rmse <- sqrt(mean((predicted_full - P8_scaled$QTY_ORDER)^2))
full_p8fit.rmse

full_results_P8.fit <- as.data.frame(cbind(P8_scaled, predicted_full))

ggplot()+
  geom_point(data = full_results_P8.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P8.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P6")+ 
  scale_colour_manual(name = " Quantidades P6", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P8.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P8.fit)
summary(full_rdois_P8.fit)$r.squared
mean(full_rdois_P8.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P8_scaled[1:2], full_results_P8.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P8_sales$QTY_ORDER))
names(results_scaled) <- c("Date", "Price", "MLP", "Quantity")

results_scaled$Price <- exp(results_scaled$Price)
results_scaled$MLP <- exp(results_scaled$MLP)
results_scaled$Quantity <- exp(results_scaled$Quantity) 

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:210, y = Quantity, color = Price,
                                          size = Price))+
        geom_line(data = results_scaled, aes(x = 1:210, y = Quantity), col = "blue")+
        geom_point(data = results_scaled, aes(x = 1:210, y = MLP, size = Price), pch = 4, col = "red")+
        xlab("Days")+
        ylab("Daily Sales Product 8")+
        ggtitle("Weekly Sales Product 8 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 10, y = 2000, label = "R^2 = 0.83")

#************************
# Removing outliers > 1000
sum(results_scaled$Quantity > 1000) # 4 dias com vendas > 1000

# Converting QTY_ORder and Price to log
P8_sales <- subset(P8_sales, P8_sales$QTY_ORDER < 1000)
P8_sales$Price <- log(P8_sales$Price)
P8_sales$QTY_ORDER <- log(P8_sales$QTY_ORDER) 

feats <- names(P8_sales)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f

#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P8_sales, 2, max)
mins <- apply(P8_sales, 2, min)
P8_scaled <- as.data.frame(scale(P8_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P8_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P8_scaled, split == TRUE)
val_nn <- subset(P8_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = seq(0.01, 0.4, 0.02), .size = seq(3, 10, 1))
P8.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F, linout = 1) 
model_p8 <- P8.fit$bestTune
model_p8

predicted <- predict(P8.fit, newdata = val_nn)
p8fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p8fit.rmse
results_P8.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
        geom_point(data = results_P8.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
        geom_point(data = results_P8.fit, aes(x = Price, y = predicted, color = "red"))+
        xlab("Preço")+
        ylab("Quantidade P8")+ 
        scale_colour_manual(name = " Quantidades P8", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P8.fit <- lm(val_resp ~ predicted, data = results_P8.fit)
summary(rdois_P8.fit)$r.squared
mean(rdois_P8.fit$residuals)
#R^2 = 0.82.

# full dataset
predicted_full <- predict(P8.fit, newdata = P8_scaled)
full_p8fit.rmse <- sqrt(mean((predicted_full - P8_scaled$QTY_ORDER)^2))
full_p8fit.rmse

full_results_P8.fit <- as.data.frame(cbind(P8_scaled, predicted_full))

ggplot()+
        geom_point(data = full_results_P8.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
        geom_point(data = full_results_P8.fit, aes(x = Price, y = predicted_full, color = "red"))+
        xlab("Preço")+
        ylab("Quantidade P6")+ 
        scale_colour_manual(name = " Quantidades P6", 
                            values =c('black'='black','red'='red'),
                            labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P8.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P8.fit)
summary(full_rdois_P8.fit)$r.squared
mean(full_rdois_P8.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P8_scaled[1:2], full_results_P8.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P8_sales$QTY_ORDER))
names(results_scaled) <- c("Date", "Price", "MLP", "Quantity")

results_scaled$Price <- exp(results_scaled$Price)
results_scaled$MLP <- exp(results_scaled$MLP)
results_scaled$Quantity <- exp(results_scaled$Quantity) 

ggplot()+
        geom_point(data = results_scaled, aes(x = 1:206, y = Quantity, color = Price,
                                              size = Price))+
        geom_line(data = results_scaled, aes(x = 1:206, y = Quantity), col = "blue")+
        geom_point(data = results_scaled, aes(x = 1:206, y = MLP, size = Price), pch = 4, col = "red")+
        xlab("Days")+
        ylab("Daily Sales Product 8")+
        ggtitle("Daily Sales Product 8 - MLP Model")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))+
        annotate("text", x = 10, y = 2000, label = "R^2 = 0.83")

