# B2WLabs | Pricing Challenge

Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 
Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 

## Describing the entire Analysis and Modeling

"Script B2W_Pric_challenge_EDA_Products.R"

1. Exploratory Data Analysis - Products. <br>

Once I had almost no idea about the business itself I started my analysis trying to get some insights about the most important product in terms of revenue and quantity sold in the entire period. <br>

Revenue: P7(60%), P2 (17%), P5 (~6%) and P8 (4.5%) <br>
Qty_Sold: P7(56%), P2 (18%), P8 (~8%) and P5 (5.5%)<br>

Product 1 <br>
Quantity sold per weekday seems to be slightly high in the middle of the week (Wednesday), but it is not a really well defined behavior. <br>
Quantity sold roughly 25 - 130 units / weekday. Sales increased in March (80 - 180 units sold / weekday) and September (50 -150 units sold / weekday). <br>
Lower prices in the first months of the year - roughly sales decreased throughout the year due to price increase. <br>

Moderated Negative Correlation Qty_Order x Price = -0.64 <br>

Product 2 <br>
Quantity sold per weekday seems to be slightly high in the middle of the week (Wednesday). <br>
Sales increased from Feb to May and July-August. <br>
Could be assigned to a seasonal behavior?? Mother's/Father's day?
Monthly Sales weak negative correlated with Prices - Correlation Qty_Order x Price = -0.45. <br>

Product 3 <br>
Roughly no tendency observed for the amount of products sold per weekday and/or throughout the year. <br>
It worth mention a small decrease in October. <br>	
Quantity sold independent on the Price. <br>
Weekday/Month Sales uncorrelated with Price - Correlation Qty_Order x Price = -0.11. <br>

Product 4 <br>
Seasonal behavior observed by the increase in the quantity ordered during the months of May and September. <br>
Regular Months = ~ 250 units / weekday. <br>
Seasonal Months = ~ 750 units / weekday. <br>
Moderated negative correlation with Prices - Correlation Qty_Order x Price = -0.50. <br>

Product 5 <br>
Quantity ordered increased enormously after July to October. <br>
Before July = ~ 30 units / weekday. <br>
After July = ~ 250 - 2000 units / weekday. <br>
Quantity ordered strongly negative correlated with Price - Correlation Qty_Order x Price = -0.82. <br>
Is there any seasonal effect additionally to the price effect? <br>

Product 6 <br>
Seasonal behavior observed by the increase in the quantity ordered during the months of February and September. <br>
Regular Months = ~ 250 units / month. <br>
Seasonal Months = ~ 1000 units / month. <br>
Monthly Sales weak negative correlated with Prices - Correlation Qty_Order x Price = -0.30. <br>

Product 7 <br>
No clear tendency can be inferred from weekdays/months sales, despite a decrease in October. <br>
October the Price is Lower and the sales descreased. ??? <br>
Weekday/Months Sales uncorrelated with Price - Correlation Qty_Order x Price = -0.22. <br>

Product 8 <br>
Quantity ordered increased enormously after June. <br>
Before June = ~ 20-30 units / weekday. <br>
After July = ~ 250 - 3000 units / weekday. <br>
Quantity ordered strongly negative correlated with Price - Correlation Qty_Order x Price = -0.87. <br>

Product 9 <br>
Seasonal behavior observed "minor" in May and big August-September. <br>
Quantity ordered strongly negative correlated with Price - Correlation Qty_Order x Price = -0.79. <br>

2. Correlation Our Prices x Competitors Prices <br>
"Script B2W_PrChall_Correlation_01.R" <br>
In this script the matrices of the prices of our products and our competitors prices pay_types 1 and 2 were built.
To calculate the correlation matrices only the variables with more than 100 observations were used.
A few outliers were found and relaced by the mean value of the variable.

General Overview of the results:

Product 1 <br>
Our prices are weakly correlated with COMPETITOR 6 Prices, PAY-TYPE's 1 and 2 equally. <br>
Correlation Price P1 x CP6 = 0.2. <br>

Product 2 <br>
Our prices are strongly correlated with COMPETITORS 3 Prices, PAY-TYPE's 1 and 2. <br>
Correlation Price P2 x CP3_PAY_TYPE_1 = 0.68. <br>
Correlation Price P2 x CP3_PAY_TYPE_2 = 0.62. <br>

Product 3 <br>
Our prices are strongly correlated with COMPETITORS 1-3 Prices, PAY-TYPE's 1 and 2 equally. <br>
Correlation Price P3 x CP1 = 0.81. <br>
Correlation Price P3 x CP2 = 0.81. <br>
Correlation Price P3 x CP3 = 0.78. <br>

Product 4 <br>
Our prices are not correlated with our COMPETITOR'S Prices. <br>

Product 5 <br>
Our prices are strongly correlated with COMPETITORS 1-3 Prices, PAY-TYPE's 1. <br>
Correlation Price P5 x CP1 = 0.71. <br>
Correlation Price P5 x CP2 = 0.73. <br>
Correlation Price P5 x CP3 = 0.72. <br>

Product 6 <br>
Our prices are strongly correlated with COMPETITORS 1 and 3, PAY-TYPE's 1. <br>
Correlation Price P6 x CP1 = 0.69. <br>
Correlation Price P6 x CP3 = 0.67. <br>

Product 7 <br>
Our prices are strongly correlated with COMPETITORS 1 - 4, PAY-TYPE's 1. <br>
Correlation Price P7 x CP1 = 0.77. <br>
Correlation Price P7 x CP2 = 0.77. <br>
Correlation Price P7 x CP3 = 0.72. <br>
Correlation Price P7 x CP4 = 0.82. <br>

Product 8 <br>
Our prices are strongly correlated with COMPETITORS 1 and 6, PAY-TYPE's 1 and 2. <br>
Correlation Price P8 x CP1 = 0.73. <br>
Correlation Price P8 x CP4 = 0.74. <br>

Product 9 <br>
Our prices are strongly correlated with COMPETITOR 1-2 and 6, PAY-TYPE's 1. <br>
Correlation Price P9 x CP1 = 0.77. <br>
Correlation Price P9 x CP2 = 0.77. <br>
Correlation Price P9 x CP6 = 0.85. <br>

Is there a particular competitor that seems more important?
Considering that the products 7, 2, 5 and 8 are the most important for our revenue and volume.
I could be thought that the COMPETITORS 1 and 3 seems to be more important to our business, because 
we are pricing our TOP volume/revenue products like these COMPETITORS.