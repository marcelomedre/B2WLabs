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

P3_sales_by_week_day_month <- read.csv("P3_sales_by_week_day_month.csv",
                                      header = TRUE, stringsAsFactors = FALSE)

P3_sales_by_week_day_month$PROD_ID <- NULL
# Exploratory Analysis
# Por Dia
P3_by_DATE <- P3_sales_by_week_day_month %>%
        group_by(DATE_ORDER) %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot()+
        geom_point(data = P3_by_DATE, aes(x = 1:250, y = QTY_ORDER, color = Price,
                                          size = Price))+
        xlab("Days")+
        ylab("Daily Sales Product 3")+
        ggtitle("Daily Sales Product 3")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

summary(P3_by_DATE)

# Por Mês
P3_by_month <- P3_sales_by_week_day_month %>%
        group_by(month) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P3_by_month, aes(x = month, y = Quantity, color = Price,
                                           size = Price))+
        geom_line(data = P3_by_month, aes(x = month, y = Quantity))+
        xlab("Month")+
        ylab("Monthly Sales Product 3")+
        ggtitle("Monthly Sales Product 3")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Por Semana
P3_by_week <- P3_sales_by_week_day_month %>%
        group_by(month, week) %>%
        summarise(Quantity = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE),
                  MeanQTY = mean(QTY_ORDER),
                  SdQTY = sd(QTY_ORDER),
                  N = length(QTY_ORDER)) %>%
        mutate(Price = REVENUE/Quantity) %>%
        mutate(SeQTY = SdQTY / sqrt(N))

ggplot()+
        geom_point(data = P3_by_week, aes(x = 1:43, y = Quantity, color = Price,
                                          size = Price))+
        geom_line(data = P3_by_week, aes(x = 1:43, y = Quantity))+
        xlab("Week")+
        ylab("Weekly Sales Product 3")+
        ggtitle("Weekly Sales Product 3")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))


# Correlation Quantidade x Preço
cor(P3_by_DATE$QTY_ORDER, P3_by_DATE$Price) # -0.26
cor(P3_by_month$Quantity, P3_by_month$Price) # -0.12
cor(P3_by_week$Quantity, P3_by_week$Price) # -0.17

# Modeling

plot(P3_by_week$Price, log(P3_by_week$Quantity)) # dia

#Removing outlier
P3_by_week <- subset(P3_by_week, log(P3_by_week$Quantity) > 1)

P3_sales <- P3_by_week[,c(1, 2, 8, 3)]

regP3 <- glm(log(Quantity) ~ ., data = P3_sales)
summary(regP3)
fit_regP3 <- predict(regP3, newdata = P3_sales, type = "response")
plot(P3_sales$Price, log(P3_sales$Quantity))
points(P3_sales$Price, fit_regP3, col = "red")

rdois_glm_opt <- lm(log(P3_sales$Quantity) ~ fit_regP3)
summary(rdois_glm_opt)$r.squared
mean(rdois_glm_opt$residuals)
# r^2 = 0.48

feats <- names(P3_sales)[-(4)]
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("Quantity ~", f))
f
#*******************************************************************************
# MLP Model
library(nnet)
maxs <- apply(P3_sales, 2, max)
mins <- apply(P3_sales, 2, min)
P3_scaled <- as.data.frame(scale(P3_sales, center = mins, scale = maxs - mins))

b <- maxs - mins
a <- mins
# Split fulldata into train and test
set.seed(1234)
split = sample.split(P3_scaled$Price, SplitRatio = 0.80)
train_nn <- subset(P3_scaled, split == TRUE)
val_nn <- subset(P3_scaled, split == FALSE)
val_resp <- val_nn$Quantity # saving test results
val_nn$Quantity <- NULL

my.grid <- expand.grid(.decay = seq(from = 0.001, to = 0.1, by = 0.01), .size = seq(5, 12, 1))
P3.fit <- train(f, data = train_nn, method = "nnet", maxit = 7000,
                preProcess = c("center", "scale"),
                tuneGrid = my.grid,
                trControl = trainControl (method = "repeatedcv",
                                          number = 10,
                                          repeats = 10,
                                          returnResamp = "final"),
                trace = F)
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
#R^2 = 0.44.

# full dataset
predicted_full <- predict(P3.fit, newdata = P3_scaled)
full_p3fit.rmse <- sqrt(mean((predicted_full - P3_scaled$Quantity)^2))
full_p3fit.rmse

full_results_P3.fit <- as.data.frame(cbind(P3_scaled, predicted_full))

ggplot()+
  geom_point(data = full_results_P3.fit, aes(x = Price, y = Quantity, color = "black"), size = 4)+
  geom_point(data = full_results_P3.fit, aes(x = Price, y = predicted_full, color = "red"))+
  xlab("Preço")+
  ylab("Quantidade P3")+ 
  scale_colour_manual(name = " Quantidades P3", 
                      values =c('black'='black','red'='red'),
                      labels = c("Observada","Prevista"))

# r2 validation sample
full_rdois_P3.fit <- lm(Quantity ~ predicted_full, data = full_results_P3.fit)
summary(full_rdois_P3.fit)$r.squared
mean(full_rdois_P3.fit$residuals)

# re-scale data
# backscaled <- P2_scaled * rep(b, each = nrow(P2_scaled)) + rep(a, each = nrow(P2_scaled))
results_sc <- cbind(P3_scaled[1:3], full_results_P3.fit$predicted_full)

results_sc <-  results_sc*rep(b, each = nrow(results_sc)) + rep (a, each = nrow(results_sc))

results_scaled <- as.data.frame(cbind(results_sc, P3_sales$Quantity))
names(results_scaled) <- c("month", "week", "Price", "MLP", "Quantity")

ggplot()+
  geom_point(data = results_scaled, aes(x = 1:41, y = Quantity, color = factor(month),
                                        group = week, size = Price))+
  geom_line(data = results_scaled, aes(x = 1:41, y = Quantity), col = "blue")+
  geom_point(data = results_scaled, aes(x = 1:41, y = MLP, size = Price), pch = 4, col = "red")+
  xlab("Week per Month")+
  ylab("Sum of Weekly Sales per Month Product 3")+
  ggtitle("Weekly Sales Product 3 - MLP Model")+
  theme(axis.text=element_text(size=12, face = "bold"),
        axis.title=element_text(size=14,face="bold"))+
  annotate("text", x = 5, y = 250, label = "R^2 = 0.77")


