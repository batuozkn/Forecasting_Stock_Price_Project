library(forecast)
install.packages("fpp")
library(fpp)

library(readxl)
data <- read_excel("~/IE360-ProjectData.xlsx")
View(data)

##UGS
UGS.ts <- ts(data$'Unleaded Gasoline Sale (UGS)', frequency=4)
UGS.ts <- UGS.ts[1:(length(UGS.ts)-4)]
plot(UGS.ts,type="l")
acf(UGS.ts,lag=8)
pacf(UGS.ts, lag=8)
plot(UGS.ts, type = "l", main = "Line Graph of UGS Time Series")


##DGS
DGS.ts <- ts(data$'Diesel Gasoline Sale (DGS)',frequency=4)
DGS.ts <- DGS.ts[1:(length(DGS.ts)-4)]
plot(DGS.ts,type="l")
acf(DGS.ts,lag=8)
pacf(DGS.ts, lag=8)
plot(DGS.ts, type = "l", main = "Line Graph of UGS Time Series")


diff_UGS.ts <- diff(UGS.ts)
plot(diff_UGS.ts, type = "l")
acf(diff_UGS.ts)
pacf(diff_UGS.ts)
library(tseries)

# Perform the ADF test
adf_test <- adf.test(diff_UGS.ts)

print(adf_test)
# ADF test says that data is stationary.





model <- auto.arima(UGS.ts, seasonal = TRUE)
model

#############################################################################
install.packages("astsa")
library(astsa)

model1<-sarima(UGS.ts,1,1,0,0,1,0,S=4,details=F)
model1
acf(resid(model1$fit), main = "ACF of residuals")
pacf(resid(model1$fit), main = "PACF of residuals")

model2<-sarima(UGS.ts,0,1,1,0,1,0,S=4,details=F)
model2
acf(resid(model2$fit), main = "ACF of residuals")
pacf(resid(model2$fit), main = "PACF of residuals")

model3<-sarima(UGS.ts,1,1,1,0,1,0,S=4,details=F)
model3
acf(resid(model3$fit), main = "ACF of residuals")
pacf(resid(model3$fit), main = "PACF of residuals")

model4<-sarima(UGS.ts,0,1,0,1,1,0,S=4,details=F)
model4
acf(resid(model4$fit), main = "ACF of residuals")
pacf(resid(model4$fit), main = "PACF of residuals")

model5<-sarima(UGS.ts,1,1,0,1,1,0,S=4,details=F)
model5
acf(resid(model5$fit), main = "ACF of residuals")
pacf(resid(model5$fit), main = "PACF of residuals")

model6<-sarima(UGS.ts,0,1,1,1,1,0,S=4,details=F)
model6
acf(resid(model6$fit), main = "ACF of residuals")
pacf(resid(model6$fit), main = "PACF of residuals")

## model5 has the lowest AIC
## AIC of model 5 is 24.79293

best_model <- model5
acf(resid(best_model$fit), main = "ACF of residuals (best model)")
pacf(resid(best_model$fit), main = "PACF of residuals (best model)")


# Load the stats package
library(stats)

# Apply the Ljung-Box test to the residuals
ljung_box_test <- Box.test(resid(best_model$fit), lag = 12, type = "Ljung-Box")

# Print the test results
print(ljung_box_test)
#p-value is 0.5497. This shows that there is no significant evidence of autocorrelation in the residuals.

#Applying Box-Pierce
box_pierce_test <- Box.test(resid(best_model$fit), lag = 12, type = "Box-Pierce")
print(box_pierce_test)
#p-value is 0.8531. This shows that there is no significant evidence of autocorrelation in the residuals.



UGS_forecast <- sarima.for(UGS.ts,4,1,1,0,1,1,0,4)
UGS_forecast

## Predicted values for 2007 sales: 
## 2007_Q1 = 678288.8
## 2007_Q2 = 896475.5
## 2007_Q3 = 948056.1
## 2007_Q4 = 826669.8


