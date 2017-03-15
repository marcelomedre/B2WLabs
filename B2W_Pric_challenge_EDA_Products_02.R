setwd("C:/Users/Marcelo/Desktop/Data/B2WLabs/")

rm(list = ls())
# This Script analysis the Total Revenue and Quantity of Products sold and split
# the data set (sales) and create a new data set for each product.

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
sales$week <- as.numeric(as.factor(week(sales$DATE_ORDER)))
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

# BarPlot - Total Rev
ggplot(sales_by_prod, aes(x = reorder(PROD_ID, -TotalRev) , y = TotalRev/1000000,
                                   fill = TotalRev, label = round(Perc,1)))+
        geom_bar(stat = "identity")+
        xlab("Product")+
        ylab("Total Revenue / Mi")+
        ggtitle("Participation of the Products in the Total Revenue")+
        geom_label(aes(fill = TotalRev), colour = "white", fontface = "bold")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# BarPlot - Number of Prod
ggplot(sales_by_prod, aes(x = reorder(PROD_ID, -TotalProd) , y = TotalProd,
                          fill = TotalProd, label = round(PercProd,1)))+
        geom_bar(stat = "identity")+
        xlab("Product")+
        ylab("Products Sold")+
        ggtitle("Products Sold")+
        geom_label(aes(fill = TotalProd), colour = "white", fontface = "bold")+
        theme(axis.text=element_text(size=12, face = "bold"),
              axis.title=element_text(size=14,face="bold"))

# Spliting sales data frame by prod_ID
temp_out <- split(sales, f = sales$PROD_ID)

P1_sales_by_week_day_month <- temp_out[[1]]
P2_sales_by_week_day_month <- temp_out[[2]]
P3_sales_by_week_day_month <- temp_out[[3]]
P4_sales_by_week_day_month <- temp_out[[4]]
P5_sales_by_week_day_month <- temp_out[[5]]
P6_sales_by_week_day_month <- temp_out[[6]]
P7_sales_by_week_day_month <- temp_out[[7]]
P8_sales_by_week_day_month <- temp_out[[8]]
P9_sales_by_week_day_month <- temp_out[[9]]

write.table(P1_sales_by_week_day_month, file = "P1_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P2_sales_by_week_day_month, file = "P2_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P3_sales_by_week_day_month, file = "P3_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P4_sales_by_week_day_month, file = "P4_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P5_sales_by_week_day_month, file = "P5_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P6_sales_by_week_day_month, file = "P6_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P7_sales_by_week_day_month, file = "P7_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P8_sales_by_week_day_month, file = "P8_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)
write.table(P9_sales_by_week_day_month, file = "P9_sales_by_week_day_month.csv",
            sep = ",", row.names = FALSE)


