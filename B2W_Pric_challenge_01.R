setwd("C:/Users/Marcelo/Desktop/Data Science/B2W Labs/")
rm(list = ls())
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

# Loading data sets

sales <- read.csv("sales.csv", header = TRUE, stringsAsFactors = FALSE)
comp_prices <- read.csv("comp_prices.csv", header = TRUE, stringsAsFactors = FALSE)


str(sales)

sales$DATE_ORDER <- as.Date(sales$DATE_ORDER)

library(ggplot2)
library(plyr)
library(dplyr)

# Looking the sales behavior during the entire period
# General overview

sales_by_day <- sales %>%
  group_by(DATE_ORDER) %>%
  summarise(TotalRev = sum(REVENUE))

ggplot(sales_by_day, aes(x = DATE_ORDER, y = TotalRev/1000000))+
  geom_point()+
  geom_smooth(method = "loess")

sales_by_day_prod <- sales %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE))

ggplot(sales_by_day_prod, aes(x = DATE_ORDER, y = TotalRev/1000000))+
  geom_line()+
  facet_grid(PROD_ID ~ .)

perc_by_day <- sales_by_day_prod

perc_by_day <- sales %>%
  group_by(DATE_ORDER, PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE)) %>%
  mutate(Perc = TotalRev/sum(TotalRev)*100)

ggplot(perc_by_day, aes(x = DATE_ORDER, y = Perc))+
  geom_line()+
  facet_grid(PROD_ID ~.)


library(highcharter)

sales_by_prod <- sales %>%
  group_by(PROD_ID) %>%
  summarise(TotalRev = sum(REVENUE)) %>%
  mutate(Perc = TotalRev/sum(TotalRev)*100)

# Creating a treemap
hchart(sales_by_prod, "treemap", hcaes(x = PROD_ID, value = Perc, color = TotalRev )) %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = c("#440154", "#21908C", "#FDE725"))) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_title(text = "Participation of the Products in the Total Revenue") %>%
  hc_legend(enabled = TRUE)

# By far P7 is the most important to the company revenue followed by P2, P5 and P8.
# Togheter they are responsible for more than 87% of the Total Revenue.


  

