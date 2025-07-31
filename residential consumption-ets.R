#Graphs and metrics: 
#Correlation and time series of residential water and electricity consumption in Joinville with ETS

#Loading libraries
library(readxl)
library(ggplot2)
library(forecast)
library(MLmetrics)

#Loading table
tab_cor <- read_excel("correlation_table_joinville.xlsx")
View(tab_cor)

#Extracting monthly total consumption vectors
a_vector <- tab_cor$`m³/month`
e_vector <- tab_cor$`MWh/month`

plot(a_vector, e_vector)

#Fitting a linear regression model
lin_model_cor <- lm(e_vector ~ a_vector, data = tab_cor)

#Summary statistics of the linear model
summary(lin_model_cor)

#Regression coefficients
coef(lin_model_cor)

#Correlation test between water and electricity consumption
cor.test(a_vector, e_vector)
resultado <- cor.test(a_vector, e_vector)
resultado$p.value  # p-value of the correlation

ggplot(data = tab_cor, aes(x = a_vector, y = e_vector)) +
  geom_point(color='black') +
  xlab("m³/month") + 
  ylab("MWh/month")

#Scatter plot with linear regression line
ggplot(data = tab_cor, aes(x = a_vector, y = e_vector)) +
  geom_point(color='black') +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  xlab("m³/month") + 
  ylab("MWh/month")

summary(a_vector)
sd(a_vector)

summary(e_vector)
sd(e_vector)

water_day <- tab_cor$`liter/inhabitant/day`
summary(water_day)

energy_day <- tab_cor$`kWh/inhabitant/day`
summary(energy_day)

cor.test(water_day,energy_day)
resultado <- cor.test(water_day,energy_day)
resultado$p.value
lin_cor <- lm(energy_day ~ water_day, data = tab_cor)
summary(lin_cor)
coef(lin_cor)
#---------------------------------------------------------  
#Time series analysis of consumption by month
  
water_ts <- ts(a_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_ts, ylab = "m³/month")
plot(decompose(water_ts))

energy_ts <- ts(e_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_ts, ylab = "MWh/month")
plot(decompose(energy_ts))

#Applying the ets model to monthly water consumption

fit <- ets(water_ts)

autoplot(forecast(fit),ylab = "m³/month")
accuracy(fit)
ts(fit)

res <- fit$residuals
plot(res)
ggAcf(res, main = "Residual series") +
  ylab("m³/month") + xlab("Time")

plot(a_vector, fit$fitted)
cor(a_vector, fit$fitted)
R2_Score(fit$fitted, a_vector)

#Applying the ets model to monthly electricity consumption

fit2 <- ets(energy_ts)

autoplot(forecast(fit2),ylab = "MWh/month")

accuracy(fit2)
ts(fit2)

res2 <- fit2$residuals
plot(res2)
ggAcf(res2, main = "Residual series") +
  ylab("MWh/month") + xlab("Time")

plot(e_vector, fit2$fitted)
cor(e_vector, fit2$fitted)
R2_Score(fit2$fitted, e_vector)

#---------------------------------------------------------  
#Time series analysis of consumption per day

water_day_ts <- ts(water_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_day_ts, ylab = "liter/inhabitant/day")

energy_day_ts <- ts(energy_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_day_ts, ylab = "kWh/inhabitant/day")

#Application of the ets model to water consumption per day

fit3 <- ets(water_day_ts)
autoplot(forecast(fit3), ylab = "liter/inhabitant/day")

accuracy(fit3)
ts(fit3)

res3 <- fit3$residuals
plot(res3)
ggAcf(res3, main = "Residual series") +
  ylab("liter/inhabitant/day") + xlab("Time")

plot(water_day, fit3$fitted)
cor(water_day, fit3$fitted)
R2_Score(fit3$fitted,water_day)

#Application of the ets model to energy consumption per day 

fit4 <- ets(energy_day_ts)
autoplot(forecast(fit4), ylab = "kWh/inhabitant/day")

accuracy(fit4)
ts(fit4)

res4 <- fit4$residuals
plot(res4)
ggAcf(res4, main = "Residual series") +
  ylab("kWh/inhabitant/day") + xlab("Time")

plot(energy_day, fit4$fitted)
cor(energy_day, fit4$fitted)
R2_Score(fit4$fitted,energy_day)
