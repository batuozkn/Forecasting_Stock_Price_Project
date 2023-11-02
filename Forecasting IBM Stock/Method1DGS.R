library(forecast)
install.packages("fpp")
library(fpp)

library(readxl)
data <- read_excel("~/IE360-ProjectData.xlsx")
View(data)

##UGS
UGS.ts <- ts(data$'Unleaded Gasoline Sale (UGS)', frequency=4)
UGS.ts <- UGS.ts[1:(length(UGS.ts)-4)]
plot(UGS.ts)
acf(UGS.ts,lag=8)
pacf(UGS.ts, lag=8)
plot(UGS.ts, type = "l", main = "Line Graph of UGS Time Series")


##DGS
DGS.ts <- ts(data$'Diesel Gasoline Sale (DGS)',frequency=4)
DGS.ts <- DGS.ts[1:(length(DGS.ts)-4)]
plot(DGS.ts)
acf(DGS.ts,lag=8)
pacf(DGS.ts, lag=8)
plot(DGS.ts, type = "l", main = "Line Graph of UGS Time Series")


diff_DGS.ts <- diff(DGS.ts)
plot(diff_DGS.ts, type = "l")
acf(diff_DGS.ts)
pacf(diff_DGS.ts)

library(tseries)

# Perform the ADF test
adf_test <- adf.test(diff_DGS.ts)

print(adf_test)
# ADF test says that data is stationary.


model <- auto.arima(DGS.ts, seasonal = TRUE)
model

#############################################################################
install.packages("astsa")
library(astsa)

model1<-sarima(DGS.ts,3,1,0,0,1,0,S=4,details=F)
model1
acf(resid(model1$fit), main = "ACF of residuals")
pacf(resid(model1$fit), main = "PACF of residuals")

model2<-sarima(DGS.ts,3,1,1,0,1,0,S=4,details=F)
model2
acf(resid(model2$fit), main = "ACF of residuals")
pacf(resid(model2$fit), main = "PACF of residuals")

model3<-sarima(DGS.ts,3,1,1,0,1,1,S=4,details=F)
model3
acf(resid(model3$fit), main = "ACF of residuals")
pacf(resid(model3$fit), main = "PACF of residuals")

model4<-sarima(DGS.ts,3,1,0,1,1,0,S=4,details=F)
model4
acf(resid(model4$fit), main = "ACF of residuals")
pacf(resid(model4$fit), main = "PACF of residuals")

model5<-sarima(DGS.ts,3,1,0,1,1,1,S=4,details=F)
model5
acf(resid(model5$fit), main = "ACF of residuals")
pacf(resid(model5$fit), main = "PACF of residuals")

model6<-sarima(DGS.ts,3,1,1,1,1,0,S=4,details=F)
model6
acf(resid(model6$fit), main = "ACF of residuals")
pacf(resid(model6$fit), main = "PACF of residuals")

## model2 has the lowest AIC
## AIC of model2 is 26.89153

best_model <- model2
acf(resid(best_model$fit), main = "ACF of residuals (best model)")
pacf(resid(best_model$fit), main = "PACF of residuals (best model)")

library(stats)

# Apply the Ljung-Box test to the residuals
ljung_box_test <- Box.test(resid(best_model$fit), lag = 12, type = "Ljung-Box")

# Print the test results
print(ljung_box_test)
#p-value is 0.94. This shows that there is no significant evidence of autocorrelation in the residuals.

#Applying Box-Pierce
box_pierce_test <- Box.test(resid(best_model$fit), lag = 12, type = "Box-Pierce")
print(box_pierce_test)
#p-value is 0.9873. This shows that there is no significant evidence of autocorrelation in the residuals.


DGS_forecast <- sarima.for(DGS.ts,4,3,1,1,0,1,0,4)
DGS_forecast

## Predicted values for 2007 sales: 
## 2007_Q1 = 3105950
## 2007_Q2 = 3968121 
## 2007_Q3 = 4087278 
## 2007_Q4 = 3808176

