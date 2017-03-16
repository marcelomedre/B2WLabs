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
library(dplyr)
library(lubridate)

P1_sales_by_week_day_month <- read.csv("P1_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P1_sales_by_week_day_month$PROD_ID <- NULL
# Exploratory Analysis
# Por Dia
P1_by_DATE <- P1_sales_by_week_day_month %>%
        group_by(DATE_ORDER) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

summary(P1_by_DATE)


ggplot()+
        geom_point(data = P1_by_DATE, aes(x = 1:252, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 1")+
        ggtitle("Daily Sales Product 1")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P1_by_DATE)
# Por Mês
P1_by_month <- P1_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P1_by_month, aes(x = month, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P1_by_month, aes(x = month, y = Quantity))+
        xlab("Month")+
        ylab("Monthly Sales Product 1")+
        ggtitle("Monthly Sales Product 1")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P1_by_week <- P1_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P1_by_week, aes(x = 1:43, y = Quantity, color = Price,
                                          size = Price))+
        geom_line(data = P1_by_week, aes(x = 1:43, y = Quantity))+
        xlab("Week")+
        ylab("Weekly Sales Product 1")+
        ggtitle("Weekly Sales Product 1")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))


# Correlation Quantidade x Preço
cor(P1_by_DATE$QTY_ORDER, P1_by_DATE$Price) # -0.51
cor(P1_by_month$Quantity, P1_by_month$Price) # -0.60
cor(P1_by_week$Quantity, P1_by_week$Price) # -0.43

# Modeling

plot(P1_by_DATE$Price, P1_by_DATE$QTY_ORDER) # dia

P1_by_DATE <- subset(P1_by_DATE, log(P1_by_DATE$QTY_ORDER) > 1)

P1_by_DATE$REVENUE <- NULL
P1_by_DATE$DATE_ORDER <- as.numeric(ymd(P1_by_DATE$DATE_ORDER))


regP1 <- glm(log(QTY_ORDER) ~ Price + DATE_ORDER, data = P1_by_DATE)
summary(regP1)
fit_regP1 <- predict(regP1, newdata = P1_by_DATE, type = "response")
plot(P1_by_DATE$Price, log(P1_by_DATE$QTY_ORDER))
points(P1_by_DATE$Price, fit_regP1, col = "red")

rdois_glm_opt <- lm(log(P1_by_DATE$QTY_ORDER) ~ fit_regP1)
summary(rdois_glm_opt)$r.squared
mean(rdois_glm_opt$residuals)
# r^2 = 0.2674

P1_sales <- P1_by_DATE
P1_sales <- P1_sales[,c(1, 3, 2)]
P1_sales$QTY_ORDER <- log(P1_sales$QTY_ORDER)

feats <- names(P1_sales)[-(3)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("QTY_ORDER ~", f))
f
#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P1_sales, 2, max)
mins <- apply(P1_sales, 2, min)
P1_scaled <- as.data.frame(scale(P1_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P1_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P1_scaled, split == TRUE)
val_nn <- subset(P1_scaled, split == FALSE)
val_resp <- val_nn$QTY_ORDER # saving test results
val_nn$QTY_ORDER <- NULL

my.grid <- expand.grid(.decay = seq(0.0001, 0.01, 0.02), .size = seq(5, 12, 1))
P1.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F)
model_p1 <- P1.fit$bestTune
model_p1

predicted <- predict(P1.fit, newdata = val_nn)
p1fit.rmse <- sqrt(mean((predicted - val_resp)^2))
p1fit.rmse
results_P1.fit <- as.data.frame(cbind(val_nn, val_resp, predicted))

ggplot()+
  geom_point(data = results_P1.fit, aes(x = Price, y = val_resp, color = "black"), size = 4)+
  geom_point(data = results_P1.fit, aes(x = Price, y = predicted, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P1")+ 
  scale_colour_manual(name = " Quantidades P1", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P1.fit <- lm(val_resp ~ predicted, data = results_P1.fit)
summary(rdois_P1.fit)$r.squared
mean(rdois_P1.fit$residuals)
#R^2 = 0.62.

# full dataset
predicted_full <- predict(P1.fit, newdata = P1_scaled)
full_p1fit.rmse <- sqrt(mean((predicted_full - P1_scaled$QTY_ORDER)^2))
full_p1fit.rmse

full_results_P1.fit <- as.data.frame(cbind(P1_scaled, predicted_full))

ggplot()+
  geom_point(data = full_results_P1.fit, aes(x = Price, y = QTY_ORDER, color = "black"), size = 4)+
  geom_point(data = full_results_P1.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P1")+ 
  scale_colour_manual(name = " Quantidades P1", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P1.fit <- lm(QTY_ORDER ~ predicted_full, data = full_results_P1.fit)
summary(full_rdois_P1.fit)$r.squared
mean(full_rdois_P1.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P1_scaled[1:2], full_results_P1.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P1_sales$QTY_ORDER))
names(results_scaled) <- c("Date", "Price", "MLP", "Quantity")

# Backtransforming QTY-ORDER
results_scaled$MLP <- exp(results_scaled$MLP)
results_scaled$Quantity <- exp(results_scaled$Quantity)

ggplot()+
  geom_point(data = results_scaled, aes(x = 1:242, y = Quantity, color = Price,
                                        size = Price))+
  geom_line(data = results_scaled, aes(x = 1:242, y = Quantity), col = "blue")+
  geom_point(data = results_scaled, aes(x = 1:242, y = MLP, size = Price), pch = 4, col = "red")+
  xlab("Days")+
  ylab("Daily Sales Product 1")+
  ggtitle("Weekly Sales Product 1 - MLP Model")+
  theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  annotate("text", x = 10, y = 100, label = "R^2 = 0.40")


