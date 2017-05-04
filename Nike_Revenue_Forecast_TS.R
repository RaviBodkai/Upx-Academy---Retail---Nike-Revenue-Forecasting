###====================================================================================================================###
#  Application      : Nike Revenue Forecast                                                                              #
#  ML Problem       : Timeseries Forecasting Model                                                                       #
#  Model Version    : 1.0                                                                                                #
#  Model Build Date : April 29, 2017                                                                                     #
#  Team             : Data Diggers                                                                                       #
#  Organization     : UPX Academy                                                                                        #
###====================================================================================================================###
#  Load Required Packages                                                                                                #
###--------------------------------------------------------------------------------------------------------------------###

library(dplyr)
library(caret)
library(readxl)
library(forecast)
library(TTR)
library(Metrics)
library(tseries)
library("astsa")

###--------------------------------------------------------------------------------------------------------------------###
#  Read in the input dataset                                                                                             #
#  - Review for its correctness                                                                                          #
#  - If not found OK, create proper dataset and format it as required                                                    #
#  - Clean the data, so that it can used as an Time Series Object                                                        #
###--------------------------------------------------------------------------------------------------------------------###

nike_revenue <- read_excel("Nike_revenue.xlsx",sheet=1,col_names = TRUE, col_types = NULL)
dim(nike_revenue)
View(nike_revenue) #Seems like both train / validation data is provided in the same dataset and some columns name are missing 
nike_data_colnames <- c("Year","Aug-31","Nov-30","Feb-28","May-31","Year","Aug-31","Nov-30","Feb-28","May-31")
colnames(nike_revenue) <- nike_data_colnames #Apply Column names to the dataset
View(nike_revenue)
str(nike_revenue)

train_nike_revenue <- nike_revenue[,1:5] #Splitting source data into train dataset
View(train_nike_revenue)
str(train_nike_revenue)
train_nike_revenue

train_nike_revenue_RC <- train_nike_revenue

val_nike_revenue <- nike_revenue[,6:10] #Splitting source data into validation dataset
View(val_nike_revenue)
str(val_nike_revenue) #Seems like it contains missing data in the rows; Also only first row seems to be valid
val_nike_revenue <- head(val_nike_revenue, 1) #selecting rows containing valid data
val_nike_revenue$Year <- as.numeric(val_nike_revenue$Year) #converting to valid column type
val_nike_revenue[,2] <- as.numeric(val_nike_revenue[,2]) #converting to valid column type
View(val_nike_revenue)
str(val_nike_revenue)

val_nike_revenue_RC <- val_nike_revenue

###====================================================================================================================###
#  Reorganize the quarterly data in Dataframe to vectorize, so that a Time Series Object can be created                  #
#  - Convert every row of a dataframe into row of vector                                                                 #
#  - Concatenate all the individual row vectors into a single continuous vector                                          #
#  - Create a Time Series Object with the vectorized row data as a quarterly frequency                                   #
###====================================================================================================================###
#  Convert every row of a dataframe into row of vector                                                                   #
###--------------------------------------------------------------------------------------------------------------------###

tsrdr1 <- as.numeric(train_nike_revenue_RC[1,2:5])
tsrdr2 <- as.numeric(train_nike_revenue_RC[2,2:5])    
tsrdr3 <- as.numeric(train_nike_revenue_RC[3,2:5])
tsrdr4 <- as.numeric(train_nike_revenue_RC[4,2:5])
tsrdr5 <- as.numeric(train_nike_revenue_RC[5,2:5])
tsrdr6 <- as.numeric(train_nike_revenue_RC[6,2:5])
tsrdr7 <- as.numeric(train_nike_revenue_RC[7,2:5])
tsrdr8 <- as.numeric(train_nike_revenue_RC[8,2:5])
tsrdr9 <- as.numeric(train_nike_revenue_RC[9,2:5])
tsrdr10 <- as.numeric(train_nike_revenue_RC[10,2:5])

###--------------------------------------------------------------------------------------------------------------------###
#  Concatenate all the individual row vectors into a single continuous vector                                            #
###--------------------------------------------------------------------------------------------------------------------###

tsrd <- as.numeric(c(tsrdr1,tsrdr2,tsrdr3,tsrdr4,tsrdr5,tsrdr6,tsrdr7,tsrdr8,tsrdr9,tsrdr10))
class(tsrd)
dim(tsrd)
tsrd  

###--------------------------------------------------------------------------------------------------------------------###
#  Create a Time Series Object with the vectorized row data as a quarterly frequency                                     #
###--------------------------------------------------------------------------------------------------------------------###

tsrd_ts <- ts(tsrd, start=1998+2/4, frequency = 4)
tsrd_ts
plot(tsrd_ts)
summary(tsrd_ts)
boxplot(tsrd_ts)

class(tsrd_ts)  # Test to ensure if object created properly as a Time Series Object
length(tsrd_ts) # Test to ensure if object created properly as a Time Series Object
tsrd_ts[40]     # Test to ensure if object created properly as a Time Series Object
dim(tsrd_ts)    # Test to ensure if object created properly as a Time Series Object
nrow(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object
ncol(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object
cycle(tsrd_ts)  # Test to ensure if object created properly as a Time Series Object
time(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object

###--------------------------------------------------------------------------------------------------------------------###
#  Similarly Create Validation Time Series Object with the vectorized row data as a quarterly frequency                  #
###--------------------------------------------------------------------------------------------------------------------###

tsrdrv <- as.numeric(val_nike_revenue_RC[1,2:5])
tsrd_val_ts <- ts(tsrdrv, start=2008+2/4, frequency = 4)
tsrd_val_ts
val_nike_revenue <- tsrd_val_ts

#####================================================================================================================#####
##  Address the case questions                                                                                          ##
##  - Plot the Time Series                                                                                              ##
##  - Time Series Components and Interpretation                                                                         ##
##    - Decompose the Time Series to identify its elements                                                              ##
##    - Interprets the Time Series Data                                                                                 ##
#####================================================================================================================#####
#   - Plot the Time Series                                                                                               #
###--------------------------------------------------------------------------------------------------------------------###

train_nike_revenue <- tsrd_ts

plot.ts(train_nike_revenue, ylab="Quarterly Revenue",main="Nike's Revenue") #plot the time series

###--------------------------------------------------------------------------------------------------------------------###
#   - Decompose the Time Series to identify its elements                                                                 #
###--------------------------------------------------------------------------------------------------------------------###

dts_nr <- decompose(train_nike_revenue) # Decompose time series to identify the elements of time series

plot(dts_nr)
dts_nr$seasonal
dts_nr$trend
dts_nr$random

lines(trendcycle(dts_nr),col="red") # fits to show the trend along the time series data

boxplot(train_nike_revenue~cycle(train_nike_revenue)) # Variance and Mean higher during November end than the other months

sdc <- stl(train_nike_revenue, s.window="periodic")   # Seasonal Trend Decomposition 
sdc

plot(sdc) #Plot revenue time series into three components - seasonal, trend and remainder
monthplot(sdc)

###--------------------------------------------------------------------------------------------------------------------###
#   - Interpret Time Series                                                                                              #
###--------------------------------------------------------------------------------------------------------------------###
#     From the plots, we notice that the time series has Seasonality, Trend and Randomness. Also we notice that the      #
#     random fluctuations are roughly constant in size over time. So this data can be describes as Additive Model.       #
#     Seasonality - We notice a repetition pattern periodically over each fiscal year with high revenues in the          #
#                   beginning of fiscal year and then revenues fall in the next two quarter and rises up until 1st       #
#                   quarter of the next fiscal year.                                                                     #
#     Trend       - We notice a constant pattern of gradual rise an upward pattern in revenues                           #
#     Random      - We notice traces of Randomness with significant high and low spikes along the time period            #
#                                                                                                                        #
#     a. The mean of the time series does not seem to be constant with time. The mean is changing along the time.        #
#     b. The variance of the series seems roughly constant along the time.                                               #
#     c. The covariance of the series seems marginally increases along the time.                                         #
#                                                                                                                        #
#     From the above (a, b, c)  it looks the series is non-stationary time series. Need to validate this.                #
###--------------------------------------------------------------------------------------------------------------------###

acf2(train_nike_revenue)  # Validation for Non-Stationary Time Series. 

###--------------------------------------------------------------------------------------------------------------------###
#  ACF/PACF plots show a quick drop-off in correlation after a small amount of lag between points. Clearly the series    #
#  is non-stationary as a high number of previous observations are correlated with future values. There are many lags    # 
#  above the significant threshold line.                                                                                 #
#                                                                                                                        #     
#  So transform the series into a stationary by differencing. Will use diff function.                                    #
#  Consider 1St Level Differencing if not does satisfy, consider 2nd Level Diffencing                                    #
###--------------------------------------------------------------------------------------------------------------------###

train_nike_revenue_diff1 = diff(train_nike_revenue)
plot(train_nike_revenue_diff1) #
acf2(train_nike_revenue_diff1)

adf.test(train_nike_revenue_diff1, alternative = "stationary") # p-Value is close to 0.2429;
#->     Augmented Dickey-Fuller Test
#-> data:  train_nike_revenue_diff1
#-> Dickey-Fuller = -2.844, Lag order = 3, p-value = 0.2429
#-> alternative hypothesis: stationary

train_nike_revenue_diff2 = diff(train_nike_revenue_diff1)
train_nike_revenue_diff2

adf.test(train_nike_revenue_diff2, alternative = "stationary") # p-Value is close to 0;
                                                               # We can conclude the object as Stationary

#->     Augmented Dickey-Fuller Test
#-> data:  train_nike_revenue_diff
#-> Dickey-Fuller = -8.9886, Lag order = 3, p-value = 0.01
#-> alternative hypothesis: stationary

# Similarly difference validation set

val_nike_revenue_diff1 <- diff(val_nike_revenue)
val_nike_revenue_diff1
val_nike_revenue_diff2 <- diff(val_nike_revenue_diff1)
val_nike_revenue_diff2

#####================================================================================================================#####
##   Part - I : Regression : y=c+mx+e. Where Intercept is c; Slope is m; and Random Error is e                          ##
#####================================================================================================================#####
##           - Perform Time Series Linear Regression - tslm                                                             ##
##           - Review Time Series Linear Regression                                                                     ##
##           - Calculate the RMSE for the Training Set                                                                  ##
##           - Predict / Forecast for the Validation Set                                                                ##
##           - Discuss Forecast - Reasonability                                                                         ##
##           - Calculate the RMSE for the Validation Set                                                                ##
##           - Predict / Forecast for Nike Revenues in Year 2010                                                        ##
#####================================================================================================================#####

plot(train_nike_revenue) # Plot time series
abline(reg=lm(train_nike_revenue~time(train_nike_revenue)))

nr_ts_lm <- tslm(train_nike_revenue ~ trend + season) # Perform the Linear Regression of the Time Series
class(nr_ts_lm)                                       # Check the class of the model

###--------------------------------------------------------------------------------------------------------------------###
#  Review Time Series Linear Regression                                                                                  #
###--------------------------------------------------------------------------------------------------------------------###

par(mfrow=c(2,2))                                     # Set canvas to graph by 2 Rows and 2 Columns
plot(nr_ts_lm)                                        # Plot the model for its results
par(mfrow=c(1,1))

summary(nr_ts_lm)                                     # Review the summary the time series model
train_nike_revenue                                    # This is actual values
nr_ts_lm$fitted.values                                # This is predicted values
nr_ts_lm$residuals                                    # This is residuals => Actual Values - Predicted Values

par(mfrow=c(3,1))                                     # Canvas to accommodate graphs in 3 Rows of 1 Column
plot(train_nike_revenue)                              # Plot of Actual Values
plot(nr_ts_lm$fitted.values)                          # Plot of Predicted Values
plot(nr_ts_lm$residuals)                              # Plot of Residuals and varies between +400 to -400 over period

par(mfrow=c(2,1))                                     # Plot for residual analysis
acf(nr_ts_lm$residuals)                               # Seems there several significant lags above threshold
                                                      # Also zeroes down at lag 2.25
pacf(nr_ts_lm$residuals)                              # Seems few significant lags above threshold
                                                      # Also zeroes down at multiple lag 1.75 and 3.75
par(mfrow=c(1,1))

###--------------------------------------------------------------------------------------------------------------------###
#  Calculate RMSE on the Training Set                                                                                    #
###--------------------------------------------------------------------------------------------------------------------###

rmse(train_nike_revenue,nr_ts_lm$fitted.values)       # RMSE of Training Set with Linear Regression Model = 222.1858

###--------------------------------------------------------------------------------------------------------------------###
#  Predict / Forecast for the Validation Set                                                                             #
###--------------------------------------------------------------------------------------------------------------------###

nr_fclm_2009 <- forecast.lm(nr_ts_lm, h=4, level=c(75,80,85,90,95), biasadj = TRUE, ts=TRUE)
summary(nr_fclm_2009)

#->  Predicted results for validation period - 2009
#->          Point Forecast    Lo 75    Hi 75    Lo 80    Hi 80    Lo 85    Hi 85    Lo 90    Hi 90    Lo 95    Hi 95
#->  2008 Q3         4738.3 4434.989 5041.611 4399.609 5076.991 4356.664 5119.936 4300.207 5176.393 4211.909 5264.691
#->  2008 Q4         4367.8 4064.489 4671.111 4029.109 4706.491 3986.164 4749.436 3929.707 4805.893 3841.409 4894.191
#->  2009 Q1         4449.8 4146.489 4753.111 4111.109 4788.491 4068.164 4831.436 4011.707 4887.893 3923.409 4976.191
#->  2009 Q2         4832.1 4528.789 5135.411 4493.409 5170.791 4450.464 5213.736 4394.007 5270.193 4305.709 5358.491

#->  Review Results with the Validation Data
#->  Year            Actual
#->  2008 Q3           5432
#->  2008 Q4           4590
#->  2009 Q1           4440 
#->  2009 Q2           4713     

#->  From the above forecast seems to reasonable at Hi 85 Confidence Intervals and forecast seems satisfactory

plot(nr_fclm_2009)                                # Plot the forecast for the validation period
lines(nr_fclm_2009$fitted, lwd=2, col="green")    # Plot the fitted values of the forecast 2009
lines(val_nike_revenue, col="red")                # Plot to see actual revenues for year 2009

par(mfrow=c(2,1))
plot(nr_fclm_2009, val_nike_revenue, type = "l")
lines(val_nike_revenue, col="red")
plot(nr_fclm_2009, val_nike_revenue, type = "h")
lines(val_nike_revenue, col="red")
par(mfrow=c(1,1))

nr_fclm_2009$mean
nr_fclm_2009$x
nr_fclm_2009$residuals
nr_fclm_2009$fitted

###--------------------------------------------------------------------------------------------------------------------###
#  Calculate RMSE on the Validation Set                                                                                  #
###--------------------------------------------------------------------------------------------------------------------###

rmse(val_nike_revenue, nr_fclm_2009$mean)             # RMSE of Validation Set = 369.0777

###--------------------------------------------------------------------------------------------------------------------###
#  Predict / Forecast for Nike Revenues in Year 2010                                                                     #
###--------------------------------------------------------------------------------------------------------------------###

nr_fclm_2010 <- forecast.lm(nr_ts_lm, h=8, level=c(75,80,85,90,95), biasadj = TRUE, ts=TRUE)
nr_fclm_2010
summary(nr_fclm_2010)

par(mfrow=c(2,1))
plot(nr_fclm_2010, type = "l")
plot(nr_fclm_2010, type = "h")
par(mfrow=c(1,1))

###--------------------------------------------------------------------------------------------------------------------###
#    Inference: From the above forecast results and considering residuals behaviour on this time series data,            #
#               Regression would not be appropriate best model for forecast as data needs further normalization.         #
###--------------------------------------------------------------------------------------------------------------------###


#####================================================================================================================#####
##   Part - II : Smoothing Methods                                                                                      ##
##   - Identify Smoothing Method for Nike's Revenue Forecasting                                                         ##
##   - Discuss over the selection of Smoothing Method                                                                   ##
#####================================================================================================================#####

#-> From the above Time Series Decompose Plot and STL Plot we have noticed and concluded that there is Seasonal Component, 
#-> Trend Component and Randomness Component in the Nike Revenue Data. Since all the three components are present in the
#-> dataset, we feel Triple Exponential Smoothing Method (TES) is appropriate to consider. TES gives lot of flexibility
#-> in controlling the Smoothing, Trend and Seasonality. 
#-> Double Exponential Smoothing Method (DES) does not perform well when Trend and Seasonality are present in the dataset.

###--------------------------------------------------------------------------------------------------------------------###
#  Triple Exponential Smoothing - Good for Non-Stationary Time Series Also                                               #
###--------------------------------------------------------------------------------------------------------------------###

nr_TES <- HoltWinters(train_nike_revenue)

nr_pred_TES <- predict(nr_TES, n.ahead=4, prediction.interval=TRUE) #Predict TES, Prediction interval gives me upper and 
                                                                    #lower bound of the confidence interval
plot(nr_pred_TES) # Observe the Prediction Interval

nr_pred_TES # Review results

#->  Predicted results for validation period - 2009
#->            fit      upr      lwr
#->  2008 Q3 5357.145 5537.584 5176.706
#->  2008 Q4 5074.670 5286.111 4863.229
#->  2009 Q1 5284.931 5545.527 5024.335
#->  2009 Q2 5815.746 6140.879 5490.612

#->  Review Results with the Validation Data
#->  Year            Actual
#->  2008 Q3           5432
#->  2008 Q4           4590
#->  2009 Q1           4440 
#->  2009 Q2           4713     

plot.ts(train_nike_revenue, xlim = c(1998,2011),ylim=c(1000,5500)) #Plot the graph
lines(nr_TES$fitted[,1],col="yellow")                              #Fit the historical fitted values
lines(nr_pred_TES[,1],col="blue")                                  #Fit the future predicted values
lines(nr_pred_TES[,2],col="red")                                   #Fit the upper interval 
lines(nr_pred_TES[,3],col="green")                                 #Fit the lower interval 

nr_pred_TES

nr_TES$alpha           # 0.3775294     Close to 0
nr_TES$beta            # 0.6180435     Close to 1 
nr_TES$gamma           # Equal to 1
nr_TES$seasonal        # Additive
nr_TES$SSE             # 310892.9
nr_TES$fitted          # Shows varying values for all the three components of TS
nr_TES$coefficients    
nr_TES$x

###====================================================================================================================###
#  Part - III : Classical Time Series Decomposition                                                                      #
#  - Two forms - Additive Decomposition and Multiplicative Decomposition                                                 #
#  - Seasonal Indices m = 4 (quarterly); m = 12 (Monthly); m = 7 (daily)                                                 #
###====================================================================================================================###

ctsd <- decompose(train_nike_revenue, type="additive")
ctsd

plot(ctsd$seasonal)
plot(ctsd$trend)
plot(ctsd$random)
plot(ctsd$figure, type="l", col="red")
plot(ctsd$x)

###====================================================================================================================###
#  Part - IV : ARIMA Models                                                                                              #
#  - A. Is the Data Stationary? Explain if Data is Stationary? Apply ideas to Nike's Revenue Data                        #
#  - B. Can Non-Stationary Data be made Stationary Data?                                                                 #
###====================================================================================================================###
# Part - IV : A - Stationary Series                                                                                      #
###--------------------------------------------------------------------------------------------------------------------###

# Stationarity is important in Time Series. Unless data is stationary, it cannot be used to build times series models. 

# For a Time Series to be Stationary 
# - Mean of the Time Series should be steady, it should not be changing over time.
# - Variance of the Time Series should be steady, it should not be changing over time.
# - Covariance of the Time Series should be steady, it  should not be changing over time.

# Tests to check Stationarity 
# - 1. ACF and PACF graphs and check significant lags
# - 2. Ljung-Box Test and examine significance evidence for Non-Zero Correlations; Smaller p-value suggest stationarity
# - 3. Augmented Dickey-Fuller (ADF) t-statistic test suggest smaller p-value for object to be stationary

###--------------------------------------------------------------------------------------------------------------------###
# 1. ACF and PACF Graphs - Check Significant lags                                                                        #  
###--------------------------------------------------------------------------------------------------------------------###

train_nike_revenue <- train_nike_revenue_diff2 # Consider the 2nd Level Differencing as the Stationary Data

Acf(train_nike_revenue)  # Seems like graph has few significant lags, and also notice it dies off between lag 9
                         # So we conclude our Nike Revenue Time Series is Stationary
Pacf(train_nike_revenue) # Seems like graph has initial few significant lags, and also notice it dies off at lag 12
                         # So we conclude our Nike Revenue Time Series is Stationary

###--------------------------------------------------------------------------------------------------------------------###
# 2. Ljung-Box Test - Check Non-Zero Correlations; Smaller p-Value                                                       #  
###--------------------------------------------------------------------------------------------------------------------###

Box.test(train_nike_revenue, lag=20, type="Ljung-Box") # Shows much smaller p-Value i.e. < 2.2e-16; 
                                                       # So we conculde our Nike Revenue Time Series is Stationary 
#->         Box-Ljung test
#->  data:  train_nike_revenue
#->  X-squared = 137.26, df = 20, p-value < 2.2e-16

###--------------------------------------------------------------------------------------------------------------------###
# 3. Augmented Dickey-Fuller (ADF) t-statistic test - Check for Smaller p-Value                                          #  
###--------------------------------------------------------------------------------------------------------------------###

adf.test(train_nike_revenue, k=4) # p-Value i.e. 0.03776 This is close to 0 indicating that series is non-stationary
                                  # So we conculde our Nike Revenue Time Series is Stationary 

#->         Augmented Dickey-Fuller Test
#->  data:  train_nike_revenue
#->  Dickey-Fuller = -3.7116, Lag order = 4, p-value = 0.03776
#->  alternative hypothesis: stationary

###--------------------------------------------------------------------------------------------------------------------###
# Part - IV : B - Non-Stationary / Stationary Series and Conversion                                                      #
###--------------------------------------------------------------------------------------------------------------------###

# There are many ways to bring a Non-stationary Time Series into Stationary Time Series. Differencing, Detrending etc
# are some methods.
# Differencing - Compute differences between consecutive observations. Also extend further with 1st level, 2nd level etc
# Detrending   - Removing TREND from a time series
# De-variance  - Taking the logarithm or square root of the Non-Constant Variance series may stabilize the variance

#####================================================================================================================#####
##  Build Forecasting Models                                                                                            ##
#####================================================================================================================#####
##  - Predict with HoltWinters - Triple Exponential Smoothing Method for Revenue Year 2010                              ##
###--------------------------------------------------------------------------------------------------------------------###

nr_pred_2010 <- predict(nr_TES, n.ahead=8, prediction.interval=TRUE)
nr_pred_2010
plot(nr_TES, nr_pred_2010) # Plot the time series alongwith predicted revenues for 2010

nr_pred_2010 <- predict(nr_TES,8) # Same as above, without intervals
nr_pred_2010
plot(nr_TES, nr_pred_2010) # Plot the time series alongwith predicted revenues for 2010

#->  Predicted results for Year - 2009 Fiscal Year
#->  2009 5357.145 5074.670 5284.931 5815.746

#->  Predicted results for Year - 2010 Fiscal Year
#->  2010 6084.891 5802.416 6012.677 6543.492

###--------------------------------------------------------------------------------------------------------------------###
##  - Predict with ARIMA       - For Revenue Year 2010                                                                  ##
###--------------------------------------------------------------------------------------------------------------------###

nr_fit_arima <- auto.arima(train_nike_revenue)
nr_fit_arima$fitted
nr_fit_arima$residuals
nr_fit_arima

tsdisplay(residuals(nr_fit_arima), lag.max=45, main='(3,0,0)(1,1,0)[4] Model Residuals')
tsdisplay(fitted(nr_fit_arima), lag.max=45, main='(3,0,0)(1,1,0)[4] Model Fitted Values')

nr_fc_2009 <- forecast(nr_fit_arima, h=4)
plot(nr_fc_2009)
nr_fc_2009
#->  Predicted results for validation period - 2009
#->          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#->  2008 Q3      -228.3626 -353.9478 -102.7773 -420.428666  -36.29645
#->  2008 Q4      -565.1425 -770.2649 -360.0201 -878.850117 -251.43489
#->  2009 Q1       505.3909  277.4467  733.3351  156.780327  854.00150
#->  2009 Q2       348.4180  115.2893  581.5466   -8.121585  704.95750

nr_fc_2010 <- forecast(nr_fit_arima, h=8)
plot(nr_fc_2010)
nr_fc_2010

#->  Predicted results for Year - 2010
#->          Point Forecast      Lo 80      Hi 80        Lo 95      Hi 95
#->  2009 Q3      -224.1413 -491.85519   43.57259  -633.574362  185.29176
#->  2009 Q4      -561.4119 -882.17534 -240.64839 -1051.977263  -70.84646
#->  2010 Q1       504.7194  155.83063  853.60811   -28.859917 1038.29865
#->  2010 Q2       348.8208  -11.00303  708.64467  -201.482275  899.12391

#####================================================================================================================#####
##  Summary of the Report                                                                                               ##
###--------------------------------------------------------------------------------------------------------------------###
#   Forecasting Approach : Prediction with HoltWinters Triple Exponential Smoothing Method and ARIMA Method              #
#                                                                                                                        #
#   Nike's Revenue data collected for fiscal years 1999 thru 2008 show that there is gradual rise in trend and           #
#   as well seasonal repetitive pattern periodically over each financial year. Repetitive rise and fall pattern          #
#   over the fiscal year is observed.                                                                                    #
#                                                                                                                        #
#   Minimum revenue was recorded at 1913 during 2nd Quarter of 1999 Fiscal Year, while Maximum revenue was               #
#   recorded at 5088 4th Quarter of 2008 Fiscal Year. Average revenues for the period is 3094.                           #
#                                                                                                                        #
#   A simple prediction / forecasting called Regression Analysis was not satifactory due to Seaonal and Trend Variances  #
#                                                                                                                        #
#   Moving ahead with other techniques including HoltWinter TES and ARIMA Methods, lead to signicant improvement in the  #
#   forecasted results. From the plots we see the results aligning the Trend along the seasonality.                      #                                                                                                                    #
#####================================================================================================================#####
