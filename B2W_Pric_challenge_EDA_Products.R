setwd("C:/Users/Marcelo/Desktop/Data Science/B2W Labs/B2WLabs/")

rm(list = ls())

# Function

rmse <- function(error)
{
        sqrt(mean(error^2))
}
# B2W LABs | Pricing Challenge

# Deliverables:
#  1) Models for Demand Forecasting: The main objective is to create a model to predict the quantity sold for
# each product given a prescribed price. Along with the statistical model, we need metrics, relationships and
# descriptions of these data in order to understand the sales behavior. What does the data tell us? How are
# the different data sources related? Is there a particular competitor that seems more important?
#
# 2) Presentation of the results: we want to know what were the steps and your strategy (approach to the
# problem) during the analysis, even if these may seem wrong. The process you went through and the
# reasoning behind it, is as important as the solutions you found. For this, please prepare a clear and
# objective presentation to explain both your methodology and your results. In case you are selected for the
# interview, you will need to make a 20-minute (max) presentation.

library(lubridate)
library(ggplot2)
library(plyr)
library(data.table)
library(dplyr)
library(caTools)
library(highcharter)

# Loading data sets

sales <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
comp_prices <- read.csv("comp_prices.csv", header = TRUE, stringsAsFactors = FALSE)

str(sales)
head(sales)
tail(sales)

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)
sales$day <- as.numeric(as.factor(wday(sales$DATE_ORDER)))
sales$month <- as.numeric(as.factor(month(sales$DATE_ORDER)))
sales$PROD_PRICE <- sales$REVENUE/sales$QTY_ORDER

sales_by_prod <- sales %>%
  group_by(PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE),
            TotalProd = sum(QTY_ORDER)) %>%
  mutate(Perc = TotalRev/sum(TotalRev)*100) %>%
  mutate(PercProd = TotalProd/sum(TotalProd)*100)

# Creating a treemap for Revenue
hchart(sales_by_prod, "treemap", hcaes(x = PROD_ID, value = Perc, color = TotalRev )) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Products Participation in the Total Revenue") %>%
  hc_legend(enabled = TRUE)
# Creating a treemap for Qty Sold
hchart(sales_by_prod, "treemap", hcaes(x = PROD_ID, value = TotalProd, color = TotalProd )) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Quantity Products Sold in the Period") %>%
  hc_legend(enabled = TRUE)

# Summarising data set TOTAL QTY_PROD and mean Price of sold products per day

sales_by_weekday_month <- sales %>%
        group_by(month, day, PROD_ID) %>%
        summarise_each(funs(sum), QTY_ORDER, REVENUE) %>%
        mutate(Price = REVENUE/QTY_ORDER)

temp_out <- split(sales_by_weekday_month, f = sales_by_weekday_month$PROD_ID)

P1_sales_by_weekday_month <- temp_out[[1]]
P2_sales_by_weekday_month <- temp_out[[2]]
P3_sales_by_weekday_month <- temp_out[[3]]
P4_sales_by_weekday_month <- temp_out[[4]]
P5_sales_by_weekday_month <- temp_out[[5]]
P6_sales_by_weekday_month <- temp_out[[6]]
P7_sales_by_weekday_month <- temp_out[[7]]
P8_sales_by_weekday_month <- temp_out[[8]]
P9_sales_by_weekday_month <- temp_out[[9]]

# Product 1
ggplot(P1_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 1")

ggplot(P1_sales_by_weekday_month, aes(x = 1:63, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 1")

# Writing file for future modeling
write.table(P1_sales_by_weekday_month, file = "P1_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P1_by_month <- sales %>%
        group_by(month, PROD_ID) %>%
        filter(PROD_ID == "P1") %>%
        summarise(QTY_ORDER = sum(QTY_ORDER),
                  REVENUE = sum (REVENUE)) %>%
        mutate(Price = REVENUE/QTY_ORDER)

ggplot(P1_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
        geom_point(size = 3)+
        xlab("Mês")+
        ylab("Quantidade vendida Produto 1")

# Tendência em ter preços menores no começo do ano, maiores no fim do ano, grosso modo
# as vendas caem ao longo do ano.

# Product 1
ggplot(P1_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 1")

ggplot(P1_sales_by_weekday_month, aes(x = 1:63, y = QTY_ORDER))+
  geom_point(data = P1_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P1_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 1")

cor(P1_sales_by_weekday_month$QTY_ORDER, P1_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P1_sales_by_weekday_month, file = "P1_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P1_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P1") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P1_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 1")

cor(P1_by_month$QTY_ORDER, P1_by_month$Price)

#*******************************************************************************
# Product 2
ggplot(P2_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P2_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P2_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 2")

ggplot(P2_sales_by_weekday_month, aes(x = 1:70, y = QTY_ORDER))+
  geom_point(data = P2_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P2_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 2")

cor(P2_sales_by_weekday_month$QTY_ORDER, P2_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P2_sales_by_weekday_month, file = "P2_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P2_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P2") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P2_by_month, aes(x = factor(month), y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 2")

cor(P2_by_month$QTY_ORDER, P2_by_month$Price)

#*******************************************************************************
# Product 3
ggplot(P3_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P3_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P3_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 3")

ggplot(P3_sales_by_weekday_month, aes(x = 1:63, y = QTY_ORDER))+
  geom_point(data = P3_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P3_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 3")

cor(P3_sales_by_weekday_month$QTY_ORDER, P3_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P3_sales_by_weekday_month, file = "P3_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P3_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P3") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P3_by_month, aes(x = factor(month), y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 3")

cor(P3_by_month$QTY_ORDER, P3_by_month$Price)
#*******************************************************************************
# Product 4
ggplot(P4_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P4_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P4_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 4")

ggplot(P4_sales_by_weekday_month, aes(x = 1:51, y = QTY_ORDER))+
  geom_point(data = P4_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P4_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 4")

cor(P4_sales_by_weekday_month$QTY_ORDER, P4_sales_by_weekday_month$Price)

  # Writing file for future modeling
write.table(P4_sales_by_weekday_month, file = "P4_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P4_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P4") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P4_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  geom_line(aes(x = month, y = QTY_ORDER), color = "red", size = 0.5)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 4")

cor(P4_by_month$QTY_ORDER, P4_by_month$Price)
#*******************************************************************************
# Product 5
ggplot(P5_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P5_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P5_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 5")

ggplot(P5_sales_by_weekday_month, aes(x = 1:49, y = QTY_ORDER))+
  geom_point(data = P5_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P5_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 5")

cor(P5_sales_by_weekday_month$QTY_ORDER, P5_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P5_sales_by_weekday_month, file = "P5_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P5_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P5") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P5_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 5")

cor(P5_by_month$QTY_ORDER, P5_by_month$Price)

#*******************************************************************************
# Product 6
ggplot(P6_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P6_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P6_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 6")

ggplot(P6_sales_by_weekday_month, aes(x = 1:70, y = QTY_ORDER))+
  geom_point(data = P6_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P6_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 6")

cor(P6_sales_by_weekday_month$QTY_ORDER, P6_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P6_sales_by_weekday_month, file = "P6_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P6_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P6") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P6_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 6")

cor(P6_by_month$QTY_ORDER, P6_by_month$Price)

#*******************************************************************************
# Product 7
ggplot(P7_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P7_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P7_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 7")

ggplot(P7_sales_by_weekday_month, aes(x = 1:70, y = QTY_ORDER))+
  geom_point(data = P7_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P7_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 7")

cor(P7_sales_by_weekday_month$QTY_ORDER, P7_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P7_sales_by_weekday_month, file = "P7_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P7_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P7") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P7_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 7")

cor(P7_by_month$QTY_ORDER, P7_by_month$Price)

#*******************************************************************************
# Product 8
ggplot(P8_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P8_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P8_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 8")

ggplot(P8_sales_by_weekday_month, aes(x = 1:56, y = QTY_ORDER))+
  geom_point(data = P8_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P8_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 8")

cor(P8_sales_by_weekday_month$QTY_ORDER, P8_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P8_sales_by_weekday_month, file = "P8_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P8_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P8") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P8_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 8")

cor(P8_by_month$QTY_ORDER, P8_by_month$Price)

#*******************************************************************************
# Product 9
ggplot(P9_sales_by_weekday_month, aes(x = day, y = QTY_ORDER))+
  geom_point(data = P9_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P9_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dia da Semana")+
  ylab("Quantidade vendida Produto 9")

ggplot(P8_sales_by_weekday_month, aes(x = 1:56, y = QTY_ORDER))+
  geom_point(data = P9_sales_by_weekday_month, aes(group = day, color = factor(month)), size = 3)+
  geom_smooth()+
  geom_line(data = P9_sales_by_weekday_month, aes(group = month, color = factor(month)))+
  xlab("Dias da Semana ao longo dos meses")+
  ylab("Quantidade vendida Produto 9")

cor(P9_sales_by_weekday_month$QTY_ORDER, P9_sales_by_weekday_month$Price)

# Writing file for future modeling
write.table(P9_sales_by_weekday_month, file = "P9_sales_by_weekday_month.csv",
            sep = ",", row.names = FALSE)

# by month
P9_by_month <- sales %>%
  group_by(month, PROD_ID) %>%
  filter(PROD_ID == "P9") %>%
  summarise(QTY_ORDER = sum(QTY_ORDER),
            REVENUE = sum (REVENUE)) %>%
  mutate(Price = REVENUE/QTY_ORDER)

ggplot(P9_by_month, aes(x = month, y = QTY_ORDER, color = Price))+
  geom_point(size = 3)+
  xlab("Mês")+
  ylab("Quantidade vendida Produto 9")

cor(P9_by_month$QTY_ORDER, P9_by_month$Price)
