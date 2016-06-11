setwd('~/RTutorial/Data') #sets working directory

mydata <- read.csv("California_Home_Prices_(2009).csv") #reads in data

mydata.lm <- lm(Price ~ Sqft, data=mydata) #linear model lm(y ~ x1 + ... + xn, data=mydata)
coeffs <- coefficients(mydata.lm); coeffs #coefficients of model

#summary(mydata.lm) #summary of fitted model

#plot(mydata.lm) #summary plots

#scatter plot with regression line
plot(mydata$Sqft, mydata$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
     xlab="Sqft", ylab="Price", pch=19)
abline(mydata.lm) #regression line

#use fitted model to predict
predict(mydata.lm, data.frame(Sqft=c(4000, 500, 6000)))

