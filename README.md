# B2WLabs | Pricing Challenge

Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 
Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 

## Describing the entire Analysis and Modeling

## 1. Exploratory Data Analysis - Products. <br>
"Script B2W_Pric_challenge_EDA_Products.R"

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

## 2. Correlation Our Prices x Competitors Prices <br>
"Script B2W_PrChall_Correlation_01.R" <br>

In this script the matrices of the prices of our products and our competitors prices pay_types 1 and 2 were built.
To calculate the correlation matrices only the variables with more than 100 observations were used.
A few outliers were found and relaced by the mean value of the variable.

## General Overview of the results:

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

## 3. Models for Demand Forecasting <br>

Product 1.
In the script "Model_Prod_1_01.R" the model to predict the quantity sold given a prescribed price
were built using an ANN type Multilayer Perceptron and a Support Vector Machine.

The starting point was a rough linear model called regP1 which presented a R^2 = 0.40.
Multilayer Perceptron - 3 hidden neurons and learnrate = 0.05, leading to a R^2 = 0.53. (Improved a little in respect to previous)
SVM model with 41 radial kernels, gamma = 0.33, cost = 1, epsilon = 0.1, presented a R^2 = 0.63 in the test dataset and
R^2 = 0.6455 in the full dataset. (The best model among the three)

Product 2.
In the script "Model_Prod_2_01.R" the model to predict the quantity sold given a prescribed price
was built using a Support Vector Machine.
The starting point was a rough linear model called regP1 which presented a R^2 = 0.28.

SVM model with 40 radial kernels, gamma = 0.33, cost = 1, epsilon = 0.1, presented a R^2 = 0.851 for the test dataset and
a RMSE = 67. For the full dataset the R^2 = 0.808 and RMSE = 18. 

Tuned SVM model 27 radial kernels, gamma = 0.33, cost = 32, epsilon = 0.29. 
test dataset: RMSE = 158, R^2 = 0.787.
full dataset: RMSE = 38, R^2 = 0.8411.(Best Model)

Product 3.
Script "Model_Prod_3_01.R"
Starting point was GLM QTY_ORDER ~ Price + month + day named regP3
Mean Residuals = -2.0e-14
R^2 = 0.523
Variable day was not significant (p-value ~ 0.57) and was removed.

Model 2. QTY_ORDER ~ Price + month - named regP3_opt
Mean Residuals = -3.0e-14
R^2 = 0.521
Model was not improved in respect to the previous one.

Model 3. MLP model was built and optimized, the best tune contains 3 hidden neurons and decay = 0.1.
RMSE-test = 25.8, R^2 = 0.77, Mean Residuals = -1.2e-15. (Improved in respect to GLM)
RMSE-full = 20.78, R^2 = 0.67, Mean Residuals = -2.0e-15.

Product 4. Seasonal Sales May and September moderately dependent on the price.
Script "Model_Prod_4_01.R"
First Model was a GLM considering the log of the QTY_ORDER:
Formula = log(QTY_ORDER) ~ Price + month + day named regP3
Mean Residuals = -2.7e-12, R^2 = 0.618 (Good Model)

Trying to get a better performance:
Model 2. MLP model was built and optimized, the best tune contains 4 hidden neurons and decay = 0.1.
RMSE-test = 2.09, R^2 = 0.67, Mean Residuals = -3.0e-17. 
RMSE-full = 1.83, R^2 = 0.78, Mean Residuals = -2.0e-17. (Better than GLM)

Product 5. Quantity ordered increases enormously after July.
Script "Model_Prod_5_01.R"
To approach the behavior of increasing sales after July, a polynomial function was
fitted to the data. The function was written as log(QTY_ORDER) ~ 2nd order poly(Price),
Intercept = -5.47e+2, a(price^1) = 2.514, b(price^2) = -4.278e-3, c(price^3) = 3.206e-6, d(price^4) = -8.943e-10.
Mean Residuals = -5.0e-18, R^2 = 0.938. 

Trying to get a better performance:
Model 2. MLP model was built and optimized, the best tune contains 7 hidden neurons and decay = 0.1.
RMSE-test = 1.84, R^2 = 0.91, Mean Residuals = -6.0e-17. 
RMSE-full = 1.45, R^2 = 0.953, Mean Residuals = -7.0e-18. (Better than Poly)

Model 3. Tuned SVM 14 radial Kernels, cost = 100, gamma = 0.1, epsilon = 0.28.
RMSE-test = 1.127, R^2 = 0.89.
RMSE-full = 1.00, R^2 = 0.941. (Similar to MLP, however, better response to the extreme values of September)

Product 6. Seasonal behavior observed by the increase in the quantity ordered during the months of February and September
Script "Model_Prod_6_01.R"
Similar seasonal behavior obsevred for product 4, so I will adopt a similar approach.
Mean Residuals = 5.65-13, R^2 = 0.470. Model fails in the extremes of the seasonal sales.

Model 2. MLP tuned - 5 hidden neurons and decay = 0.1.
RMSE-test = 72.11, R^2 = 0.8838, Mean Residuals = -1.7e-15.
RMSE-full = 34.11, R^2 = 0.85, Mean Residuals = -1.57e-15.
Great improvement in respect to the GLM one, however, 
fails in the prediction of 1 extreme value Price = 1400, Qty_order = 450.

Product 9. Seasonal behavior observed "minor" in May and big August-September.
Find an almost linear behavior of the Log(QTY-ORDER) x Price.
Fitted GLM model log(QTY_ORDER) ~ Price + month named regP9
Mean Residuals = -4.0e-15, R^2 = 0.847 (Good Model).

Model 2. MLP model, the best tuned model contained 3 hidden neurons and decay = 0.1.
RMSE-test = 1.70, R^2 = 0.852, Mean Residuals = -3.6e-17. 
RMSE-full = 1.51, R^2 = 0.89, Mean Residuals = -1.20e-17. (Better than GLM)
Fails in the prediction of 2 extreme values.