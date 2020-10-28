#Time Series Analysis on GOOGLE and MSFT Closing Stocks
## Project by Dhaval Thakur, Rushi Bhuva,Tejas Pandit


library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(fpp)
library(tsbox)
library(zoo)
library(quantmod)
library(TSstudio)

# Read stock data of Google
df_g = read.csv('C:/Users/bhuva/OneDrive - University of Waterloo/Accedemic/TERM-2/MSCI-718_Statistical Methods for Data Analytics/Assignments/Project/Data/Stocks/googl.us.txt', header = TRUE)
# Read stock data of Microsoft
df_m = read.csv('C:/Users/bhuva/OneDrive - University of Waterloo/Accedemic/TERM-2/MSCI-718_Statistical Methods for Data Analytics/Assignments/Project/Data/Stocks/msft.us.txt', header = TRUE)

#Drop the unneccessary columm
df_g = df_g[, !(names(df_g) %in% 'OpenInt')]
df_m = df_m[, !(names(df_m) %in% 'OpenInt')]

#Convert the date column to Date type
df_g$Date <- as.Date(df_g$Date, format = "%Y-%m-%d")
df_m$Date <- as.Date(df_m$Date, format = "%Y-%m-%d")

#Print summary for Google and Microsoft
summary(df_g)
summary(df_m)

#Filter by start date
df_g <- df_g %>% filter(Date >= as.Date("2005-01-01"))
df_m <- df_m %>% filter(Date >= as.Date("2005-01-01"))

#Density Plots
ggplot(df_g, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()
ggplot(df_m, aes(Close)) + geom_histogram(bins = 50, aes(y = ..density..), col = "red", fill = "red", alpha = 0.3) + geom_density()


#Mutate column Date and divide in Year and Month.
#Find mean of closing stock price for month
df_g <- df_g %>% mutate(month = as.numeric(format(Date, format = "%m")))
df_g <- df_g %>% mutate(year = as.numeric(format(Date, format = "%Y")))
df_g <- df_g %>% group_by(year,month) %>% summarize(meanClose = mean(Close))

df_m <- df_m %>% mutate(month = as.numeric(format(Date, format = "%m")))
df_m <- df_m %>% mutate(year = as.numeric(format(Date, format = "%Y")))
df_m <- df_m %>% group_by(year,month) %>% summarize(meanClose = mean(Close))

#Density Plots
ggplot(df_g, aes(meanClose)) + geom_histogram(bins = 50, aes(y = ..density..), col = "blue", fill = "blue", alpha = 0.3) + geom_density() + theme_bw()
ggplot(df_m, aes(meanClose)) + geom_histogram(bins = 50, aes(y = ..density..), col = "blue", fill = "blue", alpha = 0.3) + geom_density() + theme_bw()

#Summary and Structure of Google's Data
summary(df_g)
str(df_g)

#Summary and Structure of Microsoft's Data
summary(df_m)
str(df_m)

#Convert the data into monthly time series object with starting year as 2005
g_ts = ts(df_g$meanClose, frequency = 12, start = c(2005))
m_ts = ts(df_m$meanClose, frequency = 12, start = c(2005))

#Time-plot for Google Data
ts_plot(g_ts, Xgrid = TRUE, Ygrid = TRUE, 
        Xtitle = "Year", Ytitle = "Closing Price", 
        title = "Time Plot for Google") %>% 
  plotly::layout(yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Time-plot for Microsoft Data
ts_plot(m_ts, Xgrid = TRUE, Ygrid = TRUE, 
        Xtitle = "Year", Ytitle = "Closing Price", 
        title = "Time Plot for Microsoft") %>% 
  plotly::layout(yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Decomposition of Google's time series data
ts_decompose(g_ts) %>% 
  plotly::layout(title = "Decompositioin of Additive time Series for Google",
                 yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Decomposition of Microsoft's time series data
ts_decompose(m_ts) %>% 
  plotly::layout(title = "Decompositioin of Additive time Series for Microsoft",
                 yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Dickey-Fuller Test for Google and Microsoft's time series
adf.test(g_ts)
adf.test(m_ts)

#Default value of difference is 1
g_tsdiff1 <- diff(log(g_ts), differences=1)

#Plot the graph to check the stationarity.
ts_plot(g_ts, Xgrid = TRUE, Ygrid = TRUE, Xtitle = "Year", Ytitle = "log(Closing Price)",
        title = "Stationary Plot for Google") %>% 
  plotly::layout(yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Stationarity test of log-transformed and differenced data
adf.test(g_tsdiff1)

#Default value of difference is 1
m_tsdiff1 <- diff(log(m_ts), differences=1)

#Plot the graph to check the stationarity.
ts_plot(m_ts, Xgrid = TRUE, Ygrid = TRUE, Xtitle = "Year", Ytitle = "log(Closing Price)",
        title = "Stationary Plot for Microsoft") %>% 
  plotly::layout(yaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b",zerolinecolor = "#6b6b6b"))

#Stationarity test of log-transformed and differenced data
adf.test(m_tsdiff1)

# ACF and PACF graph for Google's Time Series data
ts_cor(g_ts, seasonal = FALSE) %>%
  plotly::layout(title = "ACF and PACF Plots for Google",
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"))

# ACF and PACF graph for Microsoft's Time Series data
ts_cor(m_ts, seasonal = FALSE) %>%
  plotly::layout(title = "ACF and PACF Plots for Microsoft",
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"))

# Divide the data into train and test data for Google
h1 <- 24
google_split <- ts_split(g_ts, sample.out = h1)
g_train <- google_split$train
g_test <- google_split$test

# auto.arima
md_g <- auto.arima(log(g_train),
                   stepwise = FALSE,
                   approximation = FALSE,
                   D = 1)

#ARIMA model for Google
md_g

#Confidence Interval of the model
confint(md_g)
fc_g <- forecast(md_g, h = h1)
fc_g$mean <- 2.718^(fc_g$mean)
fc_g$lower <- 2.718^(fc_g$lower)
fc_g$upper <- 2.718^(fc_g$upper)
fc_g$fitted <- 2.718^(fc_g$fitted)
fc_g$x <- 2.718^(fc_g$x)

#accuracy of ARIMA model for Google
accuracy(fc_g, g_test)

test_forecast(forecast.obj = fc_g, actual = g_ts, test = test, Xgrid = TRUE, Ygrid = TRUE) %>%
  plotly::layout(legend = list(x = 0.1, y = 0.95), 
                 title = "Forecasted closing stock value of Google",
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"))

#PLotting Residuals
check_res(md_g)%>% 
  plotly::layout(title = "Residual Plot for ARIMA model for Google")

#Predicted Values
fc_final_g <- forecast(md_g, h = 24)
fc_final_g$mean <- 2.718^(fc_final_g$mean)
fc_final_g$lower <- 2.718^(fc_final_g$lower)
fc_final_g$upper <- 2.718^(fc_final_g$upper)
fc_final_g$fitted <- 2.718^(fc_final_g$fitted)
fc_final_g$x <- 2.718^(fc_final_g$x)
plot_forecast(fc_final_g, title = "Forecasted values with Prediction Interval for Google", 
              Xtitle = "Year", Ytitle = "Closing Stock Price") %>% 
  plotly::layout(legend = list(x = 0.1, y = 0.95),
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"),
                 xlim)

# Divide the data into train and test data for Microsoft
h2 <- 24
microsoft_split <- ts_split(m_ts, sample.out = h2)
m_train <- microsoft_split$train
m_test <- microsoft_split$test

# auto.arima
md_m <- auto.arima(log(m_train),
                   stepwise = FALSE,
                   approximation = FALSE,
                   D = 1)

#ARIMA model for Microsoft
md_m

#Confidence Interval of Model
confint(md_m)
fc_m <- forecast(md_m, h = h2)
fc_m$mean <- 2.718^(fc_m$mean)
fc_m$lower <- 2.718^(fc_m$lower)
fc_m$upper <- 2.718^(fc_m$upper)
fc_m$fitted <- 2.718^(fc_m$fitted)
fc_m$x <- 2.718^(fc_m$x)

#Accuracy of ARIMA model for Microsoft
accuracy(fc_m, m_test)

test_forecast(forecast.obj = fc_m, actual = m_ts, test = test, Xgrid = TRUE, Ygrid = TRUE) %>%
  plotly::layout(legend = list(x = 0.1, y = 0.95), 
                 title = "Forecasted closing stock value of Microsoft", 
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"))

#Plotting Residuals
check_res(md_m) %>%
  plotly::layout(title = "Residual Plot for ARIMA model for Microsoft")

#Predicted Values
fc_final_m <- forecast(md_m, h = 24)
fc_final_m$mean <- 2.718^(fc_final_m$mean)
fc_final_m$lower <- 2.718^(fc_final_m$lower)
fc_final_m$upper <- 2.718^(fc_final_m$upper)
fc_final_m$fitted <- 2.718^(fc_final_m$fitted)
fc_final_m$x <- 2.718^(fc_final_m$x)
plot_forecast(fc_final_m, title = "Forecasted values with Prediction Interval for Microsoft", 
              Xtitle = "Year", Ytitle = "Closing Stock Price") %>% 
  plotly::layout(legend = list(x = 0.1, y = 0.95),
                 yaxis = list(linecolor = "#6b6b6b"),
                 xaxis = list(linecolor = "#6b6b6b"))
