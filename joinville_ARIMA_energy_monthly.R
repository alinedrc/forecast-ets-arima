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
  select(Date, `MWh/month`) |>   
  mutate(Month = yearmonth(Date)) |>      
  rename(Consumption = `MWh/month`) |> 
  as_tsibble(index = Month)  

# ARIMA
#Training the model using data until december 2023
train <- data |>  
  filter(year(Month) <= 2023) |>
  select(Month, Consumption)  
view(train)  

#Model fitting
fit <- train |> model(ARIMA(Consumption))

#Check the model output
report(fit)

#Selecting the forecast horizon
fc <- fit |> forecast(h = 12)
#fc <- fit |> forecast(h = 6)
#fc <- fit |> forecast(h = 3)

#Plotting the forecast without confidence intervals
fc |>  
  autoplot(train, level = NULL) +  
  labs(y = "MWh/month")

#Plotting with forecast and confidence intervals
fc |>
  autoplot(train) +
  labs(title = fit$`ARIMA(Consumption)`, y = "MWh/month") 

#Calculating residuals and fitted values
aug <-  fit |> augment()

#View the augmented data
head(aug)

#Plotting the forecast, intervals and fitted values
fc |>
  autoplot(data) +
  geom_line(data = aug, aes(y = .fitted, colour = .model)) + 
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
real <- aug$Consumption  
adjusted <- aug$.fitted

#Pearson correlation test
cor.test(real, adjusted)

#R² Calculation
SSE <- sum((real - adjusted)^2)
SST <- sum((real - mean(real))^2)
R2 <- 1 - (SSE / SST)
R2
