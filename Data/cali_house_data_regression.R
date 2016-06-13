setwd('~/RTutorial/Data') #sets working directory

mydata <- read.csv("California_Home_Prices_(2009).csv") #reads in data

#--------------------------  Basic Linear Regression
mydata.lm <- lm(Price ~ Sqft, data=mydata) #linear model lm(y ~ x1 + ... + xn, data=mydata)
coeffs <- coefficients(mydata.lm); coeffs #coefficients of model

sm <- summary(mydata.lm) #summary of fitted model
SSR <- mean(sm$residuals^2); SSR
#plot(mydata.lm) #summary plots

#scatter plot with regression line
plot(mydata$Sqft, mydata$Price, main="Price vs Sqft", col=rgb(1,0,0,0.5),
     xlab="Sqft", ylab="Price", pch=19)
abline(mydata.lm, col=rgb(0,0,1,0.5), lwd=3) #regression line


#--------------------------  Basic Linear Regression with train/test sets
#50% of the sample will be training
train_size <- floor(0.50 * nrow(mydata))

#set the random seed to make the partition reproductible
set.seed(100)

#find the row indices for the training set
train_ind <- sample(seq_len(nrow(mydata)), size = train_size)

#create training and test sets
train <- mydata[train_ind, ]
test <- mydata[-train_ind, ]

#fit linear model to training set
train.lm <- lm(Price ~ Sqft, data=train) #linear model lm(y ~ x1 + ... + xn, data=mydata)
sm <- summary(train.lm) #summary of fitted model
SSR_train <- mean(sm$residuals^2); SSR_train
r_squared <- sm$r.squared; r_squared 

#use fitted model to predict on testing set
pred <- predict(train.lm, test)
SSR_test <- mean((pred - test$Price)^2); SSR_test

#--------------------------  Muliple Linear Regression 

#fit linear model to training set
train.lm <- lm(Price ~ Sqft + Bedrooms + Bathrooms, data=train) #linear model lm(y ~ x1 + ... + xn, data=mydata)
sm <- summary(train.lm) #summary of fitted model
SSR_train <- mean(sm$residuals^2); SSR_train
r_squared <- sm$r.squared; r_squared 

#use fitted model to predict on testing set
pred <- predict(train.lm, test)
SSR_test <- mean((pred - test$Price)^2); SSR_test

#--------------------------  Muliple Linear Regression 
#fit polynomial model to training set
train.pm <- lm(Price ~ poly(Sqft, 1), data=train)
sm <- summary(train.pm) #summary of fitted model
SSR_train <- mean(sm$residuals^2); SSR_train
r_squared <- sm$r.squared; r_squared

#use fitted model to predict on testing set
pred <- predict(train.pm, test)
SSR_test <- mean((pred - test$Price)^2); SSR_test


