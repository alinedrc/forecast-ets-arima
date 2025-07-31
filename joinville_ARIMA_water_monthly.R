#Graphs and metrics:  
#Correlation and time series of residential electricity monthly consumption in Joinville with ARIMA 

#Loading libraries
library(readxl)  
library(fpp3)  

#Loading table
tab_cor <- read_excel("correlation_table_joinville.xlsx")  
view(tab_cor)  

#Transforming into a time series  
data <- tab_cor |>   
  select(Date, `m³/month`) |>   
  mutate(Month = yearmonth(Date)) |>   
  as_tsibble(index = Month)  

# ARIMA  
# Training set - up to 12/2023  

# =Train  
train <- data |>  
  filter(year(Month) <= 2023) |>  
  select(Month, `m³/month`)  
view(train)  

#Modeling  
fit <- train |> model(ARIMA(`m³/month`))  

#View model details  
report(fit)  

#Selecting the forecast horizon
fc <- fit |> forecast(h = 12)
#fc <- fit |> forecast(h = 6)
#fc <- fit |> forecast(h = 3)

#Plotting the forecast without confidence intervals
fc |>  
  autoplot(train, level = NULL) +  
  labs(y = "m³/month")   

#Plotting with forecast and confidence intervals
fc |>  
  autoplot(train) +  
  labs(title = fit$`ARIMA(m³/month)`, y = "m³/month")  

#Plotting the forecast, intervals and fitted values
fc |>
  autoplot(data) +
  geom_line(data = aug, aes(y = .fitted, colour = .model)) + 
  guides(colour = guide_legend(title = "Series"))

#Calculate and store residuals and predicted values  
aug <- fit |> augment()  

head(aug)  

# Plot forecasts, confidence interval, and adjusted values in the time series  
fc |>  
  autoplot(data) +  
  geom_line(data = aug,  
            aes(y = .fitted, colour = .model)) +   
  guides(colour = guide_legend(title = "Series"))  

#Checking residuals for model diagnostics
fit |> gg_tsresiduals()

#Autocorrelation test
aug |> features(.resid, box_pierce)

#Accuracy metrics on training data
accuracy(fit)

#Accuracy on test data for model evaluation
accuracy(fc, data)

#Correlation: Comparing real vs adjusted values  
real <- aug$`m³/month`  
adjusted <- aug$.fitted  

#Pearson correlation test  
cor.test(real, adjusted)  

#R² calculation  
SSE <- sum((real - adjusted)^2)  
SST <- sum((real - mean(real))^2)  
R2 <- 1 - (SSE / SST)  
R2  

#Analyzing predictions

#Function for linear regression (Equation 1)
reg <- function(x){
  y = 0.02562*x - 11852.7
  print(y)
}

#Working with real values
xr = c(2939424, 3008806, 3030607)
y = c(76608.093 ,79689.329, 76544.803) 

#Predicted electric energy values based on real water values
yp <- reg(xr)

#---

#Working with predicted values
fc$.mean

#Predicted electric energy values based on predicted water values
en <- reg(fc$.mean)

#R² of the predicted values (from predicted) compared to real values
cor(y,en)^2

#R² of the predicted values from real values compared to those from predicted values
cor(yp,en)^2
