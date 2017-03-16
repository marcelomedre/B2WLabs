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

P6_sales_by_week_day_month <- read.csv("P6_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P6_sales_by_week_day_month$PROD_ID <- NULL
# Exploratory Analysis
# Por Dia
P6_by_DATE <- P6_sales_by_week_day_month %>%
        group_by(DATE_ORDER) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot()+
        geom_point(data = P6_by_DATE, aes(x = 1:286, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 6")+
        ggtitle("Daily Sales Product 6")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))
summary(P6_by_DATE)

# Por Mês
P6_by_month <- P6_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P6_by_month, aes(x = month, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P6_by_month, aes(x = month, y = Quantity))+
        xlab("Month")+
        ylab("Monthly Sales Product 6")+
        ggtitle("Monthly Sales Product 6")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P6_by_week <- P6_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P6_by_week, aes(x = 1:49, y = Quantity, color = Price,
                                          size = Price))+
        geom_line(data = P6_by_week, aes(x = 1:49, y = Quantity))+
        xlab("Week")+
        ylab("Weekly Sales Product 6")+
        ggtitle("Weekly Sales Product 6")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))


# Correlation Quantidade x Preço
cor(P6_by_DATE$QTY_ORDER, P6_by_DATE$Price) # -0.20
cor(P6_by_month$Quantity, P6_by_month$Price) # -0.32
cor(P6_by_week$Quantity, P6_by_week$Price) # -0.29


# Modeling
P6_sales <- P6_by_week
P6_sales <- P6_sales[,c(1,2,8,3)]

plot(P6_sales$Price, log(P6_sales$Quantity)) # dia

regP6 <- glm(Quantity ~ ., data = P6_sales)
summary(regP6)
fit_regP6 <- predict(regP6, newdata = P6_sales, type = "response")
plot(P6_sales$Price, P6_sales$Quantity)
points(P6_sales$Price, fit_regP6, col = "red")

rdois_glm_opt <- lm(P6_sales$Quantity ~ fit_regP6)
summary(rdois_glm_opt)$r.squared
mean(rdois_glm_opt$residuals)
# r^2 = 0.19

feats <- names(P6_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("Quantity ~", f))
f
#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P6_sales, 2, max)
mins <- apply(P6_sales, 2, min)
P6_scaled <- as.data.frame(scale(P6_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P6_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P6_scaled, split == TRUE)
val_nn <- subset(P6_scaled, split == FALSE)
val_resp <- val_nn$Quantity # saving test results
val_nn$Quantity <- NULL

my.grid <- expand.grid(.decay = seq(from = 0.001, to = 0.1, by = 0.01), .size = seq(5, 12, 1))
P6.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F)
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
  ylab("Quantidade P6")+ 
  scale_colour_manual(name = " Quantidades P6", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
rdois_P6.fit <- lm(val_resp ~ predicted, data = results_P6.fit)
summary(rdois_P6.fit)$r.squared
mean(rdois_P6.fit$residuals)
#R^2 = 0.22.

# full dataset
predicted_full <- predict(P6.fit, newdata = P6_scaled)
full_p6fit.rmse <- sqrt(mean((predicted_full - P6_scaled$Quantity)^2))
full_p6fit.rmse

full_results_P6.fit <- as.data.frame(cbind(P6_scaled, predicted_full))

ggplot()+
  geom_point(data = full_results_P6.fit, aes(x = Price, y = Quantity, color = "black"), size = 4)+
  geom_point(data = full_results_P6.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P6")+ 
  scale_colour_manual(name = " Quantidades P6", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P6.fit <- lm(Quantity ~ predicted_full, data = full_results_P6.fit)
summary(full_rdois_P6.fit)$r.squared
mean(full_rdois_P6.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P6_scaled[1:3], full_results_P6.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P6_sales$Quantity))
names(results_scaled) <- c("month", "week", "Price", "MLP", "Quantity")

ggplot()+
  geom_point(data = results_scaled, aes(x = 1:49, y = Quantity, color = factor(month),
                                        group = week, size = Price))+
  geom_line(data = results_scaled, aes(x = 1:49, y = Quantity), col = "blue")+
  geom_point(data = results_scaled, aes(x = 1:49, y = MLP, size = Price), pch = 4, col = "red")+
  xlab("Week per Month")+
  ylab("Sum of Weekly Sales per Month Product 3")+
  ggtitle("Weekly Sales Product 3 - MLP Model")+
  theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  annotate("text", x = 5, y = 600, label = "R^2 = 0.61")


