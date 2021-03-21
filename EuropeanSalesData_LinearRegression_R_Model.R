getwd()
setwd("C:/Users/berkb/Documents")
#reading our data from csv file
EuropeanSales <- read.csv("EuropeanSales.csv")
#Checking the features of the dataset
attributes(EuropeanSales)
#summary of European Sales Data
summary(EuropeanSales)
#Checking if there is any missing values on the dataset
is.na(EuropeanSales)
sum(is.na(EuropeanSales))
#checking the correlations
cor(EuropeanSales$Population, EuropeanSales$SalesPerCapita)
#it seems there is an negative correlation between 
#sales per capita and Population
cor(EuropeanSales$UnemploymentRate, EuropeanSales$SalesPerCapita)
#checking the other correlations
cor(EuropeanSales$UnemploymentRate, EuropeanSales$ComputerSales)
cor(EuropeanSales$Population, EuropeanSales$ComputerSales)
#since data includes categorical variable,drop it and 
#check the correlation of all features
EuropeanSales2 = subset(EuropeanSales, select = -c(Country) )
head(EuropeanSales2)
cor(EuropeanSales2)
#Plotting the Unemployment Rate and Computer Sales
plot(EuropeanSales$UnemploymentRate, EuropeanSales$ComputerSales, xlab= "Unemployment Rate", ylab = "Computer Sales")
#Plotting the all features in the dataset
plot(EuropeanSales2)
#Model for Computer Sales on Unemployment Rate
#Education Spending and Population features
modelCSales1 <- lm(ComputerSales ~ UnemploymentRate + EducationSpending + Population, data=EuropeanSales)
summary(modelCSales1)

#Computer Sales Model attributes,coefficients and 
#sum of residuals
attributes(modelCSales1)
modelCSales1$coefficients
sum(modelCSales1$residuals)
#plotting the Computer Sales Model
par(mar = rep(2, 4))
layout(matrix(c(1,3,2,4),2,2)) 
plot(modelCSales1)

#Checking the AIC and BIC
AIC(modelCSales1)
BIC(modelCSales1)
#Second Computer Sales Model with all attributes
modelCSales2 <- lm(ComputerSales ~ UnemploymentRate + EducationSpending + Population + SalesPerCapita + GDPperHead, data=EuropeanSales)
summary(modelCSales2)

#Computer Sales Model attributes,coefficients and 
#sum of residuals
attributes(modelCSales2)
modelCSales2$coefficients
sum(modelCSales2$residuals)
#plotting the Computer Sales Model
par(mar = rep(2, 4))
layout(matrix(c(1,3,2,4),2,2)) 
plot(modelCSales2)
#checking the AIC and BIC for second model of computer 
#sales
AIC(modelCSales2)
BIC(modelCSales2)
#It seems it is better performing model to use all features
#to predict Computer Sales
#Also, AIC and BIC is lower and better

#Model for Sales Per Capita on Unemployment Rate
#Education Spending and Population features
modelSPC1 <- lm(SalesPerCapita ~ UnemploymentRate + EducationSpending + Population, data=EuropeanSales)
summary(modelSPC1)

#Sales Per Capita Model attributes and coefficients
#and sum of residuals
attributes(modelSPC1)
modelSPC1$coefficients
sum(modelSPC1$residuals)

#plotting the SalesPerCapita Model
par(mar = rep(2, 4))
layout(matrix(c(1,3,2,4),2,2)) 
plot(modelSPC1)

#Model for Sales Per Capita on Unemployment Rate
#Education Spending and Population features
modelSPC2 <- lm(SalesPerCapita ~ UnemploymentRate + EducationSpending + Population + ComputerSales + GDPperHead, data=EuropeanSales)
summary(modelSPC2)

#Sales Per Capita Model attributes and coefficients
#and sum of residuals
attributes(modelSPC2)
modelSPC2$coefficients
sum(modelSPC2$residuals)

#plotting the SalesPerCapita Model
par(mar = rep(2, 4))
layout(matrix(c(1,3,2,4),2,2)) 
plot(modelSPC2)

#It is also seen the second model is performed better
#than the first model