# ******************************************************************************************************
# Time Series Analysis - Time Series modelling on a univariate series
# ARIMA Model is used for the forecasting
#
# Data Used - UKCPI - UK Cost Index data (CPI data for 1988Q1 to 2013Q4 (UKCPI)) - UK CostIndex Inflation
# ******************************************************************************************************

rm(list = ls()) # Clear of all environment variables
options(scipen = 999) # No automatic roundoffs

# Use of external libraries
library(tseries)
library(forecast)
library(ggplot2)

# Load the data from SUVOS-TIME-CAPS
switch(Sys.info() [['sysname']],
       Windows = {ukcpids <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/UKCPI.csv", header = TRUE)},
       Linux   = {ukcpids <- read.csv("//192.168.1.33/Data/CodeMagic/Data Files/UKCPI.csv", header = TRUE)},
       Darwin  = {ukcpids <- read.csv("//Volumes/Data/CodeMagic/Data Files/UKCPI.csv", header = TRUE)})

# Check the data load
str(ukcpids)
head(ukcpids)
dim(ukcpids) # 104 observations with 2 variables
colSums(is.na(ukcpids)) # There are no missing observations

# ********** Data Exploration and Data Preparation Block *************************************** #

# Lets check the summary distribution
summary(ukcpids)

# Lets study the distribution of our variable, using a simple scatter plot
ggplot(ukcpids, aes(x = seq_along(UKCPI), y = UKCPI)) + geom_point() + xlab("Count") + ggtitle("Distibution of UKCPI")
# We see a strong increasing trend, increasing average and presence of non-stationarity

# Lets plot a histogram of our response variable to see the distribution
ggplot(ukcpids, aes(x = UKCPI)) + geom_histogram(binwidth = 5, fill = "lightblue", colour = "black")

# Lets plot the frequency distribution
ggplot(ukcpids, aes(x = UKCPI)) + geom_histogram(aes(y = ..density..), binwidth = 5, fill = "lightblue", colour = "black") + 
  stat_function(fun = dnorm, args = list(mean = mean(ukcpids$UKCPI), sd = sd(ukcpids$UKCPI)), lwd = 2, colour = "red")
# We see a normal distribution

# First we convert our response variable as a time series
# We mention the start and end period and frequency = 4, since the data is quarterly
# ukcpids$UKCPI <- ts(ukcpids$UKCPI, start = c(1988,1), end = c(2013,4), frequency = 4)

# Now we check whether the series is stationary or not...

# Perform Dickey-fuller test for stationarity
adf.test(ukcpids$UKCPI, alternative = "stationary", k = 0) # For now we are putting no additional lags (k=0)
# Since the p-value is very large, we cannot reject the NULL Hypothesis and conclude that the series has non-stationarity
# OR the series is not stationary (We dont accept the alternate hypothesis)

# Since the series is not stationary, we will attempt making the series stationary using differencing (Of orer 1 - once)
# Differencing will help in stabilising the mean of the series eliminating any trend and seasonality
d.UKCPI <- diff(ukcpids$UKCPI) # this creates a difference between consecutive observations which for this data set is quarter on
                               # quarter differencing

# to create a year on year differencing
# d.UKCPI <- diff(ukcpids$UKCPI, 4)

# Re-run the Dickey-fuller test for stationarity
adf.test(d.UKCPI, alternative = "stationary", k = 0)
# We see that the p-value 0.01 is significant and hence we reject the NULL hypothesis and conclude on the alternative hypothesis
# that the series is stationary

# Now lets plot the differenced variable and see the distribution
qplot(seq_along(d.UKCPI), d.UKCPI) + geom_line() + xlab("Count") + ggtitle("Distribution of differeced UKCPI Variable")
# Also checking the summary statistics
summary(d.UKCPI)

# Now we will analyse the ACF (Auto Correlation Function) and PACF (Partial Auto Correlation function) to decide on the
# estimation models
acf(d.UKCPI)
pacf(d.UKCPI)

# ********** Estimating & Forecasting Model **************************************************** #

# The ACF and PACF functions indicates a lag of variable 1 and could be either an AR(1) or MA(1) model or both

# ARIMA(1,0,0) - AR(1) - Auto Regressive (1) process
arima(d.UKCPI, order = c(1,0,0))

# ARIMA(0,0,1) - MA(1) - Moving Regressive (1) process
arima(d.UKCPI, order = c(0,0,1))

# ARIMA (1,1,1) - Auto Regressive (1) + Moving Average (1) with first order differencing
# We use c(1,0,1) - because we are already using the differenced variable we dont use c(1,1,1) but instead use c(1,0,1)
arima(d.UKCPI, order = c(1,0,1))

# Seeing all the three models, we chose the ARIMA(1,0,1) because of the lowest AIC value
arima.model <- arima(d.UKCPI, order = c(1,0,1))

# Lets use this model and preform a prediction for next 8 quarters
prediction <- forecast(arima.model, h = 8)

# Now lets plot this data and include the last 8 quarter of data prior to the predictions
plot(prediction, include = 8)

# Another approach for predicting and forecasting
modelprediction <- predict(arima.model, n.ahead = 20)
plot(d.UKCPI, type = "l")
lines(modelprediction$pred, col = "blue")