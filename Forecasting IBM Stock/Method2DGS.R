setwd("C:/Users/kutay/OneDrive/Masaüstü/Boğaziçi Üniversitesi/2023 Spring/IE360/Project")
library(readxl)
data<-read_excel("IE360-ProjectData.xlsx",col_names = TRUE)
data
DGS.ts<- ts(data$`Diesel Gasoline Sale (DGS)`,start=c(2000,1),end=c(2006,4),frequency=4)
plot(DGS.ts)
acf(DGS.ts)

#data is not stationary
#data seems seasonal
#we should add the trend and seasonality variable to the model

cor(data[sapply(data, is.numeric)],use = "complete.obs")

x2<- 1:32 %% 4==2
x3<- 1:32 %% 4==3
x4<- 1:32 %% 4==0

data1<-data.frame(data, s2=1*x2,s3=1*x3,s4=1*x4)
data2<-data.frame(data,quarters=1:32,s2=1*x2,s3=1*x3,s4=1*x4)
data2

lm1<- lm(Diesel.Gasoline.Sale..DGS.~s2+s3+s4,data=data1)
summary(lm1)


lm2<- lm(Diesel.Gasoline.Sale..DGS.~s2+s3+s4+quarters,data=data2)
summary(lm2)

#Check the largest absolute correlation value with DGS from the table of correlation. GNPC is the largest one.First, add the GNPC to the model


lm3<- lm(Diesel.Gasoline.Sale..DGS.~s2+s3+s4+quarters+GNP.Commerce,data=data2)
summary(lm3)


#s3 and s4 have large p value so remove them.

lm3<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total,data=data2)
summary(lm3)



model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total,data=data2)
anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+RNUV,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+X..LPG.Vehicles..NLPG.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Unleaded.Gasoline..PU.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+X..of.Diesel.Gasoline.Vehicles..NDGV.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+GNP.Agriculture,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+GNP.Commerce,data=data2))



#We selected to PG which has the smallest p-value.



model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.,data=data2)
summary(model)

#we do ANOVA test to check remove GNPT to increase significance of PG.


anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.,data=data2))
#we can not remove the GNPT from the model.

# so check anova results to determine variable wihch will be add

anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+RNUV,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+X..LPG.Vehicles..NLPG.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+Price.of.Unleaded.Gasoline..PU.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+X..of.Diesel.Gasoline.Vehicles..NDGV.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+GNP.Agriculture,data=data2))


anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2))


#GNPC is selected because the smallest p-value is in GNPC

model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2)
summary(model)

#GNPT seems unsignificant we should do ANOVA to determine remover or not 

anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Total+GNP.Commerce,data=data2))
# we can not remove PG

anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2))
# we can remove GNPT 


model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2)
summary(model)

# so check anova results to determine variable wihch will be add


anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+RNUV,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..LPG.Vehicles..NLPG.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+Price.of.Unleaded.Gasoline..PU.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..of.Diesel.Gasoline.Vehicles..NDGV.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+GNP.Agriculture,data=data2))



#select the variable which has a smallest p-value. NUGV is the smallest one.

model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
          ,data=data2)
summary(model)
#All variables seem significant but we do ANOVA to remove any variable.

anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data=data2))

#We can not remove GNPC.



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               ,data=data2))

#We can not remove PG.
#Now determine the next variable to add the model.


anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               +RNUV,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               +X..LPG.Vehicles..NLPG.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               +Price.of.Unleaded.Gasoline..PU.,data=data2))



anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               +X..of.Diesel.Gasoline.Vehicles..NDGV.,data=data2))




anova(model,lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
               +GNP.Agriculture,data=data2))


#We check all the variable and they can not add to the model because their ANOVA results seem insignificant


model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
          ,data=data2)
summary(model)

#NUGV and PG are correlated. WE should remove one of them.


model1<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2)
summary(model1)


model2<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+GNP.Commerce+X..Unleaded.Gasoline.Vehicles..NUGV.
           ,data=data2)
summary(model2)


#we should remove the NUGV



model<-model1
summary(model)



# Now we do Model Analysis
#dwtest

library(lmtest)
dwtest(model)
# alternative hypothesis : true autocorrelation is greater than 0

#no independence of residuals 

#vif test to test collinearity

library(car)
vif(model)


#no collinearity problem.


#Now we should check model assumptions.


plot(model)

#we should log transformation due to linearity problem

model<-lm(log(Diesel.Gasoline.Sale..DGS.)~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2)
summary(model)

plot(model)

#we should do log transformations to other variables because assumptions are not satisfied.

model<-lm(log(Diesel.Gasoline.Sale..DGS.)~s2+quarters+log(Price.of.Diesel.Gasoline..PG.)+log(GNP.Commerce),data=data2)
summary(model)

plot(model)


#Now constant variance and normality assumptions seem better in this model.

model<-lm(Diesel.Gasoline.Sale..DGS.~s2+quarters+Price.of.Diesel.Gasoline..PG.+GNP.Commerce,data=data2)
modelDGS<-model

#correlation between residuals and explanatory variable

y_predict<-predict(model,data2)
y_act<-data$`Diesel Gasoline Sale (DGS)`
residuals<- y_act - y_predict
cor(data$`Diesel Gasoline Sale (DGS)`,residuals,  use = "complete.obs")

#there is no correlation between residuals amd explonatory variables.


model<-lm(log(Diesel.Gasoline.Sale..DGS.)~s2+quarters+log(Price.of.Diesel.Gasoline..PG.)+log(GNP.Commerce),data=data2)


predict2007Q1 <- data.frame(s2=0,quarters=29,Price.of.Diesel.Gasoline..PG.=449.1909,GNP.Commerce=4857305,12539562)
predict(model,predict2007Q1,level = 0.9,interval = "prediction")

exp(14.9292)



predict2007Q2 <- data.frame(s2=1,quarters=30,Price.of.Diesel.Gasoline..PG.=449.1909,GNP.Commerce=5852403,78328151)
predict(model,predict2007Q2,level = 0.9,interval = "prediction")

exp(15.15688)





predict2007Q3 <- data.frame(s2=0,quarters=31,Price.of.Diesel.Gasoline..PG.=449.1909,GNP.Commerce=7480414,37162258)
predict(model,predict2007Q3,level = 0.9,interval = "prediction")

exp(15.25533)




predict2007Q4 <- data.frame(s2=0,quarters=32,Price.of.Diesel.Gasoline..PG.=449.1909,GNP.Commerce=6397744,76104164)
predict(model,predict2007Q4,level = 0.9,interval = "prediction")

exp(15.15272)

