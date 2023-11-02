setwd("C:/Users/kutay/OneDrive/Masaüstü/Boğaziçi Üniversitesi/2023 Spring/IE360/Project")

library(readxl)
data<-read_excel("IE360-ProjectData.xlsx",col_names = TRUE)
data
UGS.ts<- ts(data$'Unleaded Gasoline Sale (UGS)',start=c(2000,1),end=c(2006,4),frequency=4)
UGS.ts
plot(UGS.ts)
cor(data[sapply(data, is.numeric)], use = "complete.obs")

x2<- 1:32 %% 4 == 2
x3<- 1:32 %% 4 == 3
x4<- 1:32 %% 4 == 0
data1<- data.frame(data,s2 = 1*x2,s3=1*x3,s4=1*x4)
data1
data2<- data.frame(data,Quarters = 1:32,s2 = 1*x2,s3=1*x3,s4=1*x4)
data2

lm1 <- lm(Unleaded.Gasoline.Sale..UGS. ~ s2 + s3 + s4, data = data1)
summary(lm1)

lm2<- lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters,data = data2)
summary(lm2)

#Check the largest absolute correlation value with UGS from the table of correlation. NUGV is the largest one.First, add the NUGV to the model

lm3<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..Unleaded.Gasoline.Vehicles..NUGV.,data = data2)
summary(lm3)

#Even though NUGV is the largest correlation value with UGS , When we look at the summmary NUGV seems unsignificant so try another one with the second largest correlation value which is NLPG

lm4<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.,data = data2)
summary(lm4)

#NLPG seems significant so we added NLPG to the model. We will decide what we should add to the model afterwards by doing anova
#We will go with the model which has the smallest p value

model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.,data = data2)
anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+RNUV,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+Price.of.Unleaded.Gasoline..PU.,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+Price.of.Diesel.Gasoline..PG.,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+X..Unleaded.Gasoline.Vehicles..NUGV.,data = data2))



anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+X..of.Diesel.Gasoline.Vehicles..NDGV.,data = data2))



anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+GNP.Agriculture,data = data2))



anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+GNP.Commerce,data = data2))



anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+GNP.Total
               ,data = data2))


#PU has the lowest p value. So we update the model by adding PU to model.

model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+Price.of.Unleaded.Gasoline..PU.,data = data2)
summary(model)

# PU looks like insignificant so we want to remove it from the model. But we should check it by removing NLPG from the model to see whether PU is significant or not ?

anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Unleaded.Gasoline..PU.,data = data2))

#p value is so small so we can not remove NLPG or add PU to the model.
#Now we will add the variable with second smallest P value obtained from ANOVA which is RNUV


model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+X..LPG.Vehicles..NLPG.+RNUV,data = data2)

summary(model)

#RNUV seems insignificant so we should check to remove NLPG with anova.

anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV,data = data2))

#P value is large enough to remove NLPG.


model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV,data = data2)
summary(model)

#RNUV is significant. We will look the ANOVA results to decide if we update the model with any variable.


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+Price.of.Diesel.Gasoline..PG.,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+X..of.Diesel.Gasoline.Vehicles..NDGV.,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+GNP.Agriculture,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+GNP.Commerce,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+GNP.Total
               ,data = data2))



#PG has the Smallest p value. So we should add the PG to the model


model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV,data = data2)
summary(model)

#PG is significant but we still want to try removing RNUV from the model to see if it is going better.

anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.,data = data2))


#P value is so small so we can not remove RNUV.


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data = data2))




anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+X..of.Diesel.Gasoline.Vehicles..NDGV.,data = data2))




anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+GNP.Agriculture,data = data2))


anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+GNP.Commerce,data = data2))



anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+GNP.Total
               ,data = data2))




#We added to smallest p value GNPC to the model.


model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+RNUV+GNP.Commerce,data = data2)
summary(model)

#GNPC is insignificant yet we still want to check if removing the others will make any difference

#Removing RNUV
anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data = data2))

# we can not remove RNUV from the model with this smallest p value


#Removing PG
anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+GNP.Commerce,data = data2))

# we can not remove PG from the model with this smallest p value


#Removing both
anova(model,lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+GNP.Commerce,data = data2))



# we can not remove PG and RNUV from the model with this smallest p value.



#We did not add GNPC to the model. We should stop there because GNPC which has the best ANOVA result is not significant. So we don't have to try other ones.



model<-lm(Unleaded.Gasoline.Sale..UGS.~s2+s3+s4+Quarters+RNUV+Price.of.Diesel.Gasoline..PG.,data = data2)
summary(model)

#All variables are significant.We have a trend and seasonality variables.R-square values are 0.9534 and 0.9401 and p value is 6.808e-13 which are so satisfying.
#We will move on by checking model assumptions.

UGS_model<- model



library(car)
library(lmtest)

vif(UGS_model)

#since there is no value high enough to indicate there exist a multicollinearity in our model


dwtest(UGS_model)
#dw value is so close to 2 and p value is 0.5329. Based on these results, we do not have sufficient evidence to reject the null hypothesis that the true autocorrelation is equal to 0.


plot(UGS_model)

#When we look at the plot we can say that our linearity assumption holds.

#Normality assumption seems to be correct, since the data follows the theortical line.

#Due to the lack of constancy in the average value across the range of the independent variable, it is evident that the assumption of constant variance is not satisfied. As a result, it is necessary to apply a logarithmic transformation to the independent variable.


model<-lm(log(Unleaded.Gasoline.Sale..UGS.)~s2+s3+s4+Quarters+RNUV+Price.of.Diesel.Gasoline..PG.,data=data2)

plot(model)

#The average value of standardized residuals appears to remain consistent across the range of fitted values, indicating that the assumption of constant variance is fulfilled.Linearity assumption does not maintain we should take the log of the explaining variables.

model<-lm(log(Unleaded.Gasoline.Sale..UGS.)~s2+s3+s4+Quarters+log(Price.of.Diesel.Gasoline..PG.)+log(RNUV),data=data2)
plot(model)

#Linearity assumption is maintained. 
#Normality assumption is maintained.
#constant variance is maintained 

#After that we should check the correlation between the explanatory variable and the residuals.



y_predict<-predict(model,data2)
y_act<-log(data$'Unleaded Gasoline Sale (UGS)')
residuals<- y_act - y_predict

cor(data$'Unleaded Gasoline Sale (UGS)',residuals,  use = "complete.obs")

#Correlation between explanatory variables and residuals is 0.1966 which is insignificant. So our model satisfied the all model assumptions.
#We will make predicts for 2007 based on our model

predict2007Q1 <- data.frame(s2=0,s3=0,s4=0,Quarters=29,Price.of.Diesel.Gasoline..PG.=449.1909,RNUV=0.007386855)
predict(model,predict2007Q1,level = 0.9,interval = "prediction")

exp(13.49022)



predict2007Q2 <- data.frame(s2=1,s3=0,s4=0,Quarters=30,Price.of.Diesel.Gasoline..PG.=449.1909,RNUV=0.010591663)
predict(model,predict2007Q2,level = 0.9,interval = "prediction")


exp(13.6137)


predict2007Q3 <- data.frame(s2=0,s3=1,s4=0,Quarters=31,Price.of.Diesel.Gasoline..PG.=449.1909,RNUV=0.010077553)
predict(model,predict2007Q3,level = 0.9,interval = "prediction")


exp(13.74766)


predict2007Q4 <- data.frame(s2=0,s3=0,s4=1,Quarters=32,Price.of.Diesel.Gasoline..PG.=449.1909,RNUV=0.012371491)
predict(model,predict2007Q4,level = 0.9,interval = "prediction")


exp(13.55011)

#######################

