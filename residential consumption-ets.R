#Graphs and metrics: 
#Correlation and time series of residential water and electricity consumption in Joinville with ETS

library(readxl)
library(ggplot2)
library(forecast)
library(MLmetrics)

tab_cor <- read_excel("correlation_table_joinville.xlsx")

a_vector <- tab_cor$`m³/month`
e_vector <- tab_cor$`MWh/month`

plot(a_vector, e_vector)

# Fits a linear regression model between the vectors a_vector and e_vector
lin_model_cor <- lm(e_vector ~ a_vector, data = tab_cor)

# Displays the regression model summary
summary(lin_model_cor)

# Retrieves the model coefficients
coef(lin_model_cor)

# Correlation test between water and electricity consumption
cor.test(a_vector, e_vector) 
resultado <- cor.test(a_vector, e_vector)

# Displays the p-value of the correlation test
resultado$p.value

# Scatter plot of water and electricity consumption
ggplot(data = tab_cor, aes(x = a_vector, y = e_vector)) +
  geom_point(color='black') +
  xlab("m³/month") + 
  ylab("MWh/month")

# Scatter plot with trend line (linear regression)
ggplot(data = tab_cor, aes(x = a_vector, y = e_vector)) +
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  xlab("m³/month") + 
  ylab("MWh/month")

# Descriptive statistics of water consumption
summary(a_vector)
sd(a_vector)

# Descriptive statistics of electricity consumption
summary(e_vector)
sd(e_vector)

# Retrieves daily water and electricity consumption data
water_day <- tab_cor$`liter/inhabitant/day`
summary(water_day)

energy_day <- tab_cor$`kWh/inhabitant/day`
summary(energy_day)

#---------------------------------------------------------  
# Time series analysis of monthly consumption

# Converts water consumption data to a time series
water_ts <- ts(a_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_ts, ylab = "m³/month")

# Decomposes the water consumption time series
plot(decompose(water_ts))

# Converts electricity consumption data to a time series
energy_ts <- ts(e_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_ts, ylab = "MWh/month")

# Decomposes the electricity consumption time series
plot(decompose(energy_ts))

#---------------------------------------------------------  
# Applies the ETS model to monthly water consumption

fit <- ets(water_ts)

# Forecast plot generated by the ETS model
autoplot(forecast(fit),ylab = "m³/month")

# Evaluates the accuracy of the ETS model
accuracy(fit)

# Converts the forecast to a time series
ts(fit)

# Retrieves and plots the ETS model residuals
res <- fit$residuals
plot(res)

# Residuals autocorrelation
acf(res,main = "Residual series",
    xlab = "Time",
    ylab = "m³/month")

# Comparison between observed and fitted values
plot(a_vector, fit$fitted)

# Correlation between observed and fitted values
cor(a_vector, fit$fitted)

# R² calculation to evaluate model fit
R2_Score(fit$fitted, a_vector)

#---------------------------------------------------------  
# Applies the ETS model to monthly electricity consumption

fit2 <- ets(energy_ts)

# Forecast plot generated by the ETS model
autoplot(forecast(fit2),ylab = "MWh/month")

# Evaluates the accuracy of the ETS model
accuracy(fit2)

# Converts the forecast to a time series
ts(fit2)

# Retrieves and plots the ETS model residuals
res2 <- fit2$residuals
plot(res2)

# Residuals autocorrelation
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "MWh/month")

# Comparison between observed and fitted values
plot(e_vector, fit2$fitted)

# Correlation between observed and fitted values
cor(e_vector, fit2$fitted)

# R² calculation to evaluate model fit
R2_Score(fit2$fitted, e_vector)

#---------------------------------------------------------  
# Time series analysis of daily consumption

# Converts daily water consumption data to a time series
water_day_ts <- ts(water_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_day_ts, ylab = "liter/inhabitant/day")

# Converts daily electricity consumption data to a time series
energy_day_ts <- ts(energy_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_day_ts, ylab = "kWh/inhabitant/day")

#---------------------------------------------------------  
# Applies the ETS model to daily water consumption

fit2 <- ets(water_day_ts)
autoplot(forecast(fit2), ylab = "liter/inhabitant/day")

# Evaluates the accuracy of the ETS model
accuracy(fit2)

# Converts the forecast to a time series
ts(fit2)

# Retrieves and plots the ETS model residuals
res2 <- fit2$residuals
plot(res2)

# Residuals autocorrelation
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "liter/inhabitant/day")

# Comparison between observed and fitted values
plot(e_vector, fit2$fitted)

# Correlation between observed and fitted values
cor(e_vector, fit2$fitted)

# R² calculation to evaluate model fit
R2_Score(fit2$fitted, e_vector)

#---------------------------------------------------------  
# Applies the ETS model to daily electricity consumption

fit2 <- ets(energy_day_ts)
autoplot(forecast(fit2), ylab = "kWh/inhabitant/day")

# Evaluates the accuracy of the ETS model
accuracy(fit2)

# Converts the forecast to a time series
ts(fit2)

# Retrieves and plots the ETS model residuals
res2 <- fit2$residuals
plot(res2)

# Residuals autocorrelation
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "kWh/inhabitant/day")

# Comparison between observed and fitted values
plot(e_vector, fit2$fitted)

# Correlation between observed and fitted values
cor(e_vector, fit2$fitted)

# R² calculation to evaluate model fit
R2_Score(fit2$fitted, e_vector)
