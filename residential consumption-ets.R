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

lin_model_cor <- lm(e_vector ~ a_vector, data = tab_cor)

summary(lin_model_cor)
coef(lin_model_cor)

cor.test(a_vector, e_vector) #correlation between water and electricity consumption
resultado <- cor.test(a_vector, e_vector)
resultado$p.value

ggplot(data = tab_cor, aes(x = a_vector, y = e_vector)) +
  geom_point(color='black') +
  xlab("m³/month") + 
  ylab("MWh/month")

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
resultado <- cor.test(a_vector, e_vector)
resultado$p.value
lin_cor <- lm(energy_day ~ water_day, data = tab_cor)
summary(lin_cor)
coef(lin_cor)
#---------------------------------------------------------  
#time series analysis of consumption by month
  
water_ts <- ts(a_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_ts, ylab = "m³/month")
plot(decompose(water_ts))

energy_ts <- ts(e_vector, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_ts, ylab = "MWh/month")
plot(decompose(energy_ts))

#applying the ets model to monthly water consumption

fit <- ets(water_ts)

autoplot(forecast(fit),ylab = "m³/month")
accuracy(fit)
ts(fit)

res <- fit$residuals
plot(res)
acf(res,main = "Residual series",
    xlab = "Time",
    ylab = "m³/month")

plot(a_vector, fit$fitted)
cor(a_vector, fit$fitted)
R2_Score(fit$fitted, a_vector)

#applying the ets model to monthly electricity consumption

fit2 <- ets(energy_ts)

autoplot(forecast(fit2),ylab = "MWh/month")

accuracy(fit2)
ts(fit2)

res2 <- fit2$residuals
plot(res2)
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "MWh/month")

plot(e_vector, fit2$fitted)
cor(e_vector, fit2$fitted)
R2_Score(fit2$fitted, e_vector)

#---------------------------------------------------------  
#time series analysis of consumption per day

water_day_ts <- ts(water_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(water_day_ts, ylab = "liter/inhabitant/day")

energy_day_ts <- ts(energy_day, start=c(2013, 1), end=c(2024, 3), frequency=12)
autoplot(energy_day_ts, ylab = "kWh/inhabitant/day")

#application of the arima model to water consumption per day

fit2 <- ets(water_day_ts)
autoplot(forecast(fit2), ylab = "liter/inhabitant/day")

accuracy(fit2)
ts(fit2)

res2 <- fit2$residuals
plot(res2)
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "liter/inhabitant/day")

plot(e_vector, fit2$fitted)
cor(e_vector, fit2$fitted)
R2_Score(fit2$fitted, e_vector)

#application of the arima model to energy consumption per day 

fit2 <- ets(energy_day_ts)
autoplot(forecast(fit2), ylab = "kWh/inhabitant/day")

accuracy(fit2)
ts(fit2)

res2 <- fit2$residuals
plot(res2)
acf(res2,main = "Residual series",
    xlab = "Time",
    ylab = "kWh/inhabitant/day")

plot(e_vector, fit2$fitted)
cor(e_vector, fit2$fitted)
R2_Score(fit2$fitted, e_vector)