# B2WLabs | Pricing Challenge

Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 
Script: B2W_Pric_challenge_EDA_Products.R contains the Exploratory Analysis of the Products 

## Describing the entire Analysis and Modeling

## 1. Exploratory Data Analysis - Products. <br>
Script "B2W_Pric_challenge_EDA_Products_02.R"
Analyses the Total Revenue and Quantity of Products sold and splits
the data set (sales), creating a new data set for each product.

Once I had almost no idea about the business itself I started my analysis trying to get some insights about the most important product in terms of revenue and quantity sold in the entire period. <br>

Revenue: P7(60%), P2 (17%), P5 (~6%) and P8 (4.5%) <br>
Qty_Sold: P7(56%), P2 (18%), P8 (~8%) and P5 (5.5%)<br>

"Script B2W_Pric_challenge_EDA_Products.R"
Analyses each product individually, unless a different script is referenced.

* Product 1 <br>
Quantity sold per weekday seems to be slightly high in the middle of the week (Wednesday), but it is not a really well defined behavior. <br>
Quantity sold roughly 25 - 130 units / weekday. Sales increased in March (80 - 180 units sold / weekday) and September (50 -150 units sold / weekday). <br>
Lower prices in the first months of the year - roughly sales decreased throughout the year due to price increase. <br>

Moderated Negative Correlation Qty_Order x Price = -0.64 <br>

* Product 2 <br> - Script "Model_Prod_2_02.R"
Quantity sold per weekday seems to be slightly high in the middle of the week (Wednesday). <br>
Seasonality - Sales increased from Feb to May and July-August. <br>
Correlation Qty_Order x Price = Negative and Moderate
Daily basis: - 0.44
Weekly basis: - 0.46
Monthly basis:  -0.38

* Product 3 <br>
Roughly no tendency observed for the amount of products sold per weekday and/or throughout the year. <br>
It worth mention a small decrease in October. <br>	
Quantity sold independent on the Price. <br>
Weekday/Month Sales uncorrelated with Price - Correlation Qty_Order x Price = -0.11. <br>

* Product 4 <br>
Seasonal behavior observed by the increase in the quantity ordered during the months of May and September. <br>
Regular Months = ~ 250 units / weekday. <br>
Seasonal Months = ~ 750 units / weekday. <br>
Moderated negative correlation with Prices - Correlation Qty_Order x Price = -0.50. <br>

* Product 5 <br> - Script "Model_Prod_5_02.R"
Quantity ordered increased enormously from July to October. <br>
Quantity ordered strongly negatively correlated with Price.
Is there any seasonal effect additionally to the price effect? <br>
Correlation Qty_Order x Price = Negative and Moderate
Daily basis: - 0.61
Weekly basis: - 0.68
Monthly basis:  -0.82

* Product 6 <br>
Seasonal behavior observed by the increase in the quantity ordered during the months of February and September. <br>
Regular Months = ~ 250 units / month. <br>
Seasonal Months = ~ 1000 units / month. <br>
Monthly Sales weak negative correlated with Prices - Correlation Qty_Order x Price = -0.30. <br>

* Product 7 - Script "Model_Prod_7_01.R"
No clear tendency can be inferred from weekdays/months sales, despite a decrease in October. <br>
October the Price is Lower and the sales descreased. ??? <br>
Correlation Qty_Order x Price
Daily basis: - 0.58
Weekly basis: - 0.64
Monthly basis: - 0.87

* Product 8 <br> - Script "Model_Prod_8_01.R"
Quantity ordered increased enormously after June. <br>
Before June = ~ 20-30 units / weekday. <br>
After July = ~ 250 - 3000 units / weekday. <br>
Quantity ordered strongly dependent on the Price decrease.
Correlation Qty_Order x Price
Daily basis: - 0.38
Weekly basis: - 0.23
Monthly basis:  0.008

* Product 9 <br> - Script "Model_Prod_9_02.R"
Seasonal behavior observed "minor" in May and big August-September. <br>
Quantity ordered strongly negative correlated with Price <br>
Correlation Qty_Order x Price
Daily basis: - 0.58
Weekly basis: - 0.79
Monthly basis: - 0.67

## 2. Correlation Our Prices x Competitors Prices <br>
"Script B2W_PrChall_Correlation_01.R" <br>

In this script the matrices of the prices of our products and our competitors prices pay_types 1 and 2 were built.
To calculate the correlation matrices only the variables with more than 100 observations were used.
A few outliers were found and relaced by the mean value of the variable.

## General Overview of the results:

* Product 1 <br>
Our prices are weakly correlated with COMPETITOR 6 Prices, PAY-TYPE's 1 and 2 equally. <br>
Correlation Price P1 x CP6 = 0.2. <br>

* Product 2 <br>
Our prices are strongly correlated with COMPETITORS 3 Prices, PAY-TYPE's 1 and 2. <br>
Correlation Price P2 x CP3_PAY_TYPE_1 = 0.68. <br>
Correlation Price P2 x CP3_PAY_TYPE_2 = 0.62. <br>

* Product 3 <br>
Our prices are strongly correlated with COMPETITORS 1-3 Prices, PAY-TYPE's 1 and 2 equally. <br>
Correlation Price P3 x CP1 = 0.81. <br>
Correlation Price P3 x CP2 = 0.81. <br>
Correlation Price P3 x CP3 = 0.78. <br>

* Product 4 <br>
Our prices are not correlated with our COMPETITOR'S Prices. <br>

* Product 5 <br>
Our prices are strongly correlated with COMPETITORS 1-3 Prices, PAY-TYPE's 1. <br>
Correlation Price P5 x CP1 = 0.71. <br>
Correlation Price P5 x CP2 = 0.73. <br>
Correlation Price P5 x CP3 = 0.72. <br>

* Product 6 <br>
Our prices are strongly correlated with COMPETITORS 1 and 3, PAY-TYPE's 1. <br>
Correlation Price P6 x CP1 = 0.69. <br>
Correlation Price P6 x CP3 = 0.67. <br>

* Product 7 <br> 
Our prices are strongly correlated with COMPETITORS 1 - 4, PAY-TYPE's 1. <br>
Correlation Price P7 x CP1 = 0.77. <br>
Correlation Price P7 x CP2 = 0.77. <br>
Correlation Price P7 x CP3 = 0.72. <br>
Correlation Price P7 x CP4 = 0.82. <br>

* Product 8 <br>
Our prices are strongly correlated with COMPETITORS 1 and 6, PAY-TYPE's 1 and 2. <br>
Correlation Price P8 x CP1 = 0.73. <br>
Correlation Price P8 x CP4 = 0.74. <br>

* Product 9 <br>
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

* Product 2.
Script "Model_Prod_2_02.R" 
The starting point was a GLM model called regP2 with formula = log(Quantity) ~ Price + week
R^2 = 0.72.
MLP model optimized with 3 hidden neurons and decay rate 0.06.
R^2-test = 0.80.
R^2-full = 0.72.

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

* Product 5. Quantity ordered increases enormously after July.
Script "Model_Prod_5_02.R"
To approach the behavior of increasing sales after July, a exponential decay was
fitted to the data. The function was written as QTY_ORDER ~ exp(a+b*Price),
a = 21.741818, b = -0.021346
Mean Residuals = -3.0e-16, R^2 = 0.84. 

Trying to get a better performance:
MLP model was built and optimized, the best tune contains 7 hidden neurons and decay = 0.01.
RMSE-test = 0.0458, R^2 = 0.96, Mean Residuals = -3.0e-19. 
RMSE-full = 0.066, R^2 = 0.89, Mean Residuals = -1.2e-19.

Tuned SVM 62 radial Kernels, cost = 512, gamma = 0.1, epsilon = 0.1
RMSE-test = 7.33, R^2 = 0.94
RMSE-full = 4.2, R^2 = 0.88 (Similar to MLP, however, overpredicting high values)

Product 6. Seasonal behavior observed by the increase in the quantity ordered during the months of February and September
Script "Model_Prod_6_01.R"
Similar seasonal behavior obsevred for product 4, so I will adopt a similar approach.
Mean Residuals = 5.65-13, R^2 = 0.470. Model fails in the extremes of the seasonal sales.

Model 2. MLP tuned - 5 hidden neurons and decay = 0.1.
RMSE-test = 72.11, R^2 = 0.8838, Mean Residuals = -1.7e-15.
RMSE-full = 34.11, R^2 = 0.85, Mean Residuals = -1.57e-15.
Great improvement in respect to the GLM one, however, 
fails in the prediction of 1 extreme value Price = 1400, Qty_order = 450.

* Product 7. Apparently no seasonal behavior
Script "Model_Prod_7_01.R"
MLP Model, 15 hidden neuron and decay = 0.03. (Formula = Quantity ~ Price + week)
R^2 = 0.46, Mean Residuals = 3.3e-18.

SVM Model 34 Support Vectors, cost = 100, gamma = 0.01, epsilon = 0.15
RMSE-test = 364, R^2 = 0.63, Mean Residuals = -6e-14.
RMSE-full = 7.21, R^2 = 0.43, Mean Residuals = 7e-14.

* Product 8. Quantity ordered increased enormously after June strongly correlated with price.
Script "Model_Prod_8_01.R"
The starting point was a GLM model called regP8 with formula = log(Quantity) ~ log(Price) + (day)
R^2 = 0.78.

MLP model optimized with 8 hidden neurons and decay rate 0.01.
R^2-test = 0.82.
R^2-full = 0.83.

MLP model optimized with 9 hidden neurons and decay rate 0.01. (Days > 1000 sales removed. 4 in 210 observations)
R^2-test = 0.82.
R^2-full = 0.83.

* Product 9. Seasonal behavior observed "minor" in May and big August-September.
Script "Model_Prod_9_02.R"
Find an almost linear behavior of the Log(QTY-ORDER) x Price.
Fitted GLM model log(QTY_ORDER) ~ Price + DATE_ORDER named regP9
Mean Residuals = 4.0e-18, R^2 = 0.6847

MLP model, the best tuned model contained 9 hidden neurons and decay = 0.01.
RMSE-test = 0.101, R^2 = 0.84, Mean Residuals = -3.6e-17. 
RMSE-full = 0.092, R^2 = 0.81, Mean Residuals = -1.20e-17. (Better than GLM)
Fails in the prediction of 2 extreme values.