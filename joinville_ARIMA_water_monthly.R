# Graphs and metrics:  
# Correlation and time series of residential water monthly consumption in Joinville with ARIMA  

library(readxl)  
library(fpp3)  

tab_cor <- read_excel("correlation_table_joinville.xlsx")  
view(tab_cor)  

# Transforming into a time series  
data <- tab_cor |>   
  select(Date, `m³/month`) |>   
  mutate(Month = yearmonth(Date)) |>   
  as_tsibble(index = Month)  

# ARIMA  
# Training set - up to 12/2023  
# Forecast for 12 months  

# Train  
train <- data |>  
  filter(year(Month) <= 2023) |>  
  select(Month, `m³/month`)  
view(train)  

# Modeling  
fit <- train |> model(ARIMA(`m³/month`))  

# View model details  
report(fit)  

# Generate forecasts  
fc <- fit |> forecast(h = 12)  

fc |>  
  autoplot(train, level = NULL) +  
  labs(y = "m³/month")   

# Generate the plot with the correct model title  
fc |>  
  autoplot(train) +  
  labs(title = fit$`ARIMA(m³/month)`, y = "m³/month")  

# Calculate and store residuals and predicted values  
aug <- fit |> augment()  

head(aug)  

# Plot forecasts, confidence interval, and adjusted values in the time series  
fc |>  
  autoplot(data) +  
  geom_line(data = aug,  
            aes(y = .fitted, colour = .model)) +   
  guides(colour = guide_legend(title = "Series"))  

# Check residuals  
fit |> gg_tsresiduals()  

# Some autocorrelation tests  
aug |> features(.resid, box_pierce)  

# Accuracy - training set  
accuracy(fit)  

# Accuracy - test set - to evaluate the model  
accuracy(fc, data)  

# Correlation  
# Extract actual and adjusted values  
real <- aug$`m³/month`  
adjusted <- aug$.fitted  

# Pearson correlation test  
cor.test(real, adjusted)  

# R-squared calculation  
# Compute SSE and SST  
SSE <- sum((real - adjusted)^2)  
SST <- sum((real - mean(real))^2)  

# R²  
R2 <- 1 - (SSE / SST)  
R2  
