# B2WLabs | Pricing Challenge

## Deliverables:
Models for Demand Forecasting: The main objective is to create a model to predict the quantity sold for
each product given a prescribed price. Along with the statistical model, we need metrics, relationships and
descriptions of these data in order to understand the sales behavior. What does the data tell us? How are
the different data sources related? Is there a particular competitor that seems more important?
Describing the entire Analysis and Modeling

## 1. Exploratory Data Analysis - Products. <br>
Script "B2W_Pric_challenge_EDA_Products_02.R"
Analyses the Total Revenue and Quantity of Products sold and splits
the data set (sales), creating a new data set for each product.

Once I had almost no idea about the business itself I started my analysis trying to get some insights about the most 
important product in terms of revenue and quantity sold in the entire period. <br>

Revenue: P7(60%), P2 (17%), P5 (~6%) and P8 (4.5%) <br>
Qty_Sold: P7(56%), P2 (18%), P8 (~8%) and P5 (5.5%)<br>

Analyses each product individually, the script containing its Exploratory Data Analysis and <br>
Modeling procedure is presented in the comments of each product. <br>

* Product 1 <br> - Script "Model_Prod_1_02.R" <br>
Sales seems to be regular in the entire period up to September. <br>
It decreases in October, probably relate to the Price increase. <br>
Lower prices in the first months of the year - roughly sales decreased throughout the year due to price increase. <br>
Correlation Qty_Order x Price = Negative and Moderate <br>
Daily basis: - 0.51 <br>
Weekly basis: - 0.43 <br>
Monthly basis:  -0.60 <br>

* Product 2 <br> - Script "Model_Prod_2_02.R" <br>
Quantity sold per weekday seems to be slightly high in the middle of the week (Wednesday). <br>
Seasonality - Sales increased from Feb to May and July-August. <br>
Correlation Qty_Order x Price = Negative and Moderate <br>
Daily basis: - 0.44 <br>
Weekly basis: - 0.46 <br>
Monthly basis:  -0.38 <br>

* Product 3 <br> - Script "Model_Prod_3_02.R" <br>
Roughly no tendency observed for the amount of products sold per day throughout the year. <br>
In a Month/week basis, a decrease is observed during the analysed period. <br>
Quantity sold roughly independent on the Price. <br>
Correlation Sales x Price = weak. <br>
Daily basis: - 0.26 <br>
Weekly basis: - 0.17 <br>
Monthly basis:  -0.12 <br>

* Product 4 <br> - Script "Model_Prod_4_02.R" <br>
Seasonal behavior observed by the increase in the quantity ordered during the months of May and September. <br>
Regular Months = ~ 2000 units / month. <br>
Seasonal Months = > 5000 units / month. <br>
Moderated negative correlation with Prices <br>
Correlation Qty_Order x Price <br>
Daily basis: - 0.51 <br>
Weekly basis: - 0.61 <br>
Monthly basis:  -0.55 <br>

* Product 5 <br> - Script "Model_Prod_5_02.R" <br>
Quantity ordered increased enormously from July to October. <br>
Quantity ordered strongly negatively correlated with Price.
Is there any seasonal effect additionally to the price effect? <br>
Correlation Qty_Order x Price = Negative and Strong <br>
Daily basis: - 0.61 <br>
Weekly basis: - 0.68 <br>
Monthly basis:  -0.82 <br>

* Product 6 <br> - Script "Model_Prod_6_02.R" <br>
Mean sales = 15 units /day. <br>
10 obs. (286 obs.) sales > 50 units. <br>
Promotional sales in one week? Weekly sales > 600 units. <br>
Sales weakly negative correlated with Prices. <br>
Correlation Qty_Order x Price <br>
Daily basis: - 0.20 <br>
Daily basis without 10 sales > 50 units /day = -0.34. <br>
Weekly basis: - 0.29 <br>
Monthly basis: - 0.32 <br>

* Product 7 - Script "Model_Prod_7_01.R"
By far the most important product in Revenue and Sales (~ 60%). <br>
No clear tendency can be inferred from weekdays/months sales, despite a decrease in October. <br>
October the Price is Lower and the sales descreased. ??? <br>
Monthly Sales strongly negative correlated with Prices.
Correlation Qty_Order x Price <br>
Daily basis: - 0.58 <br>
Weekly basis: - 0.64 <br>
Monthly basis: - 0.87 <br>

* Product 8 <br> - Script "Model_Prod_8_01.R"
Quantity ordered increased enormously after June. <br>
Before June = ~ 20-30 units / weekday. <br>
After July = ~ 250 - 3000 units / weekday. <br>
Quantity ordered strongly dependent on the Price decrease. <br>
Correlation Qty_Order x Price <br>
Daily basis: - 0.38 <br>
Weekly basis: - 0.23 <br>
Monthly basis:  0.008 <br>

* Product 9 <br> - Script "Model_Prod_9_02.R"
Seasonal behavior observed "minor" in May and big August-September. <br>
Quantity ordered strongly negative correlated with Price <br>
Correlation Qty_Order x Price <br>
Daily basis: - 0.58 <br>
Weekly basis: - 0.79 <br>
Monthly basis: - 0.67 <br>

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

* Product 1.
Script "Model_Prod_2_01.R" the model to predict the quantity <br>
sold given a prescribed price were built using an ANN type Multilayer Perceptron ^
R^2 = 0.40. Modeling the sales behavior of this product is trick because <br>
there are several Quantity sold with similar prices. The use of additional variables <br>
could improve greatly the prediction, such as, group the sales by week/months.

For a weekday/month group a Linear model "Model_Prod_1_01.R" <br>
Called regP1 presented a R^2 = 0.40 and a Multilayer Perceptron with <br>
3 hidden neurons and learnrate = 0.05 improved the R^2 to 0.53. <br>
Finally, SVM model with 41 radial kernels, gamma = 0.33, cost = 1, epsilon = 0.1, <br>
presented a R^2 = 0.63 in the test dataset and R^2 = 0.6455 in the full dataset. <br>
The best model among the three)

* Product 2.
Script "Model_Prod_2_02.R" <br>
The starting point was a GLM model called regP2, <br>
formula = log(Quantity) ~ Price + week <br>
R^2 = 0.72. <br>
MLP model optimized with 3 hidden neurons and decay rate 0.06.<br>
R^2-test = 0.80.<br>
R^2-full = 0.72.<br>

* Product 3.
Script "Model_Prod_3_02.R" <br>
Starting point was GLM QTY_ORDER ~ Price + month + week named regP3 <br>
Mean Residuals = -6.0e-18 <br>
R^2 = 0.44 <br>

MLP model was built and optimized, the best tune contained 8 hidden neurons and decay = 0.001. <br>
RMSE-full = 0.12, R^2 = 0.77, Mean Residuals = 5.0e-15.

* Product 4. Seasonal Sales May and September moderately dependent on the price. <br>
Script "Model_Prod_4_02.R" <br>
First Model was a GLM considering the log of the QTY_ORDER: <br>
Formula = log(QTY_ORDER) ~ Price + day named regP4 <br>
Mean Residuals = -2.7e-12, R^2 = 0.65 (Good Model) <br>

Trying to get a better performance:
Model 2. MLP model was built and optimized, the best tune contains 4 hidden neurons and decay = 0.1.
RMSE-test = 0.14, R^2 = 0.65, Mean Residuals = 6.0e-18. <br>
RMSE-full = 0.10, R^2 = 0.78, Mean Residuals = -1.0e-17. <br>
Model fails for outliers and small volume changes. 

* Product 5. Quantity ordered increases enormously after July. <br>
Script "Model_Prod_5_02.R" <br>
To approach the behavior of increasing sales after July, <br> 
a exponential decay was fitted to the data.<br>
The function was written as QTY_ORDER ~ exp(a+b*Price),<br>
a = 21.741818, b = -0.021346 <br>
Mean Residuals = -3.0e-16, R^2 = 0.84. <br>

Trying to get a better performance: <br>
MLP model was built and optimized, the best MLP contained 7 hidden neurons and decay = 0.01. <br>
RMSE-test = 0.0458, R^2 = 0.96, Mean Residuals = -3.0e-19. <br>
RMSE-full = 0.066, R^2 = 0.89, Mean Residuals = -1.2e-19.<br>
MLP model fitted very well to the data, including the seasonality sales. <br>

Tuned SVM 62 radial Kernels, cost = 512, gamma = 0.1, epsilon = 0.1 <br>
RMSE-test = 7.33, R^2 = 0.94 <br>
RMSE-full = 4.2, R^2 = 0.88 (Similar to MLP, however, overpredicting high values) <br>

* Product 6. Seasonal behavior observed in some weeks. <br> 
Script "Model_Prod_6_01.R"
Weekday approach. <br> 
GLM Model (Quantity ~ month + day + Price) Mean Residuals = 5.65-13. <br>
R^2 = 0.470. Model fails in the extremes of the seasonal sales. <br>

MLP tuned - 5 hidden neurons and decay = 0.1. <br>
RMSE-test = 17, R^2 = 0.8838, Mean Residuals = -1.7e-15.
RMSE-full = 34.11, R^2 = 0.85, Mean Residuals = -1.57e-15.
Great improvement in respect to the GLM one, however, 
fails in the prediction of 1 extreme value Price = 1400, Qty_order = 450.

Script "Model_Prod_6_02.R"
Sales grouped by week. <br> 
GLM Model (Quantity ~ month + week + Price). <br>
Mean Residuals = 5.65-13. <br>
R^2 = 0.270.<br>

MLP tuned - 5 hidden neurons and decay = 0.1. <br>
RMSE-test = 0.061, R^2 = 0.22, Mean Residuals = 2.4e-18.
RMSE-full = 0.11, R^2 = 0.61, Mean Residuals = -8.24e-18.

* Product 7. Apparently no seasonal behavior <br>
Script "Model_Prod_7_01.R" <br>
MLP Model, 15 hidden neuron and decay = 0.03. (Formula = Quantity ~ Price + week) <br>
R^2 = 0.46, Mean Residuals = 3.3e-18. <br>
Model responds well to the data that is scattered, however, it was not capable of predict
extreme values. <br>

SVM Model 34 Support Vectors, cost = 100, gamma = 0.01, epsilon = 0.15 <br>
RMSE-test = 364, R^2 = 0.63, Mean Residuals = -6e-14. <br>
RMSE-full = 7.21, R^2 = 0.43, Mean Residuals = 7e-14. <br>

* Product 8. Quantity ordered increased enormously after June strongly correlated with price. <br>
Script "Model_Prod_8_01.R" <br>
The starting point was a GLM model called regP8 with formula = log(Quantity) ~ log(Price) + (day) <br>
R^2 = 0.78. <br>

MLP model optimized with 8 hidden neurons and decay rate 0.01. <br>
R^2-test = 0.82. <br>
R^2-full = 0.83. <br>

MLP model optimized with 9 hidden neurons and decay rate 0.01. <br>
(Days > 1000 sales removed. 4 in 210 observations) <br>
R^2-test = 0.82. <br>
R^2-full = 0.83. <br>
The Model could be optimized increasing its performance with more data points. <br>

* Product 9. Seasonal behavior observed "minor" in May and big August-September. <br>
Script "Model_Prod_9_02.R" <br>
Find an almost linear behavior of the Log(QTY-ORDER) x Price. <br>
Fitted GLM model log(QTY_ORDER) ~ Price + DATE_ORDER named regP9 <br>
Mean Residuals = 4.0e-18, R^2 = 0.6847 <br>

MLP model, the best tuned model contained 9 hidden neurons and decay = 0.01. <br>
RMSE-test = 0.101, R^2 = 0.84, Mean Residuals = -3.6e-17. <br> 
RMSE-full = 0.092, R^2 = 0.81, Mean Residuals = -1.20e-17. (Better than GLM) <br>
Fails in the prediction of few extreme values. <br>

Seasonal Sales could be divided into two problems. <br>
Modeling would be less complex and could predict the Sales Better.


