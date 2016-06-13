library(MASS)
library(calibrate)
setwd('~/RTutorial/Data') #sets working directory
mydata <- read.csv("California_Home_Prices_(2009).csv") #reads in data

#------------------------ Histogram of prices with pdf curve
h<-truehist(mydata$Price, prob=TRUE, col=rgb(1,0,0,0.5), ylim=c(0, 1.2*exp(-13)), xlab="Price", ylab="Normalized Frequency", border="white", main="Normalized Price Histogram") 
lines(density(mydata$Price), col="blue", lwd=2)

#------------------------ Pdf of weighted die
weighted_die = c(0.1, 0.2, 0.3, 0.25, 0.1, 0.05)
barplot(weighted_die, width=1, col=rgb(1,0,0,0.5), xlab="Face", ylab="Probability", border="white", main="Probabilities of Rolling a Weighted Die")

#------------------------ Pdf of normal dist
mean=100; sd=15
x <- seq(from=-4, to=4, by=0.1)*sd + mean
hx <- dnorm(x,mean,sd)
plot(x, hx, type="l", col=rgb(1,0,0,0.5), lwd=3, xlab="Values of RV", ylab="Density", main="Normal Distribution")

#------------------------ Pdf of uniform dist
x <- seq(from=-2, to=2, by=0.1)
uni <- function(x){
  if (x < -1 || x >1){
    return(0)
  }
  else{
    return(1)
  }
}
y <- sapply(x, uni)
plot(y, col=rgb(1,0,0,0.5), type="l", lwd=3, xlab="Values of RV", ylab="Density", main="Uniform Distribution")

#------------------------ Pdf of exponential dist
x <- seq(0,1000,length=100)
plot(dexp(x, rate = 0.01, log = FALSE), type="l", col=rgb(1,0,0,0.5), lwd=3, xlab="Values of RV", ylab="Density", main="Exponential Distribution")

#------------------------ Histogram of Res compared with that of P|P|m, A, b, Res
set.seed(124)

norm <- rnorm(1000)
hist(norm, breaks=20, col=rgb(0,0,1,0.5), xlab="Res", ylab="Frequency", main="Gaussian Noise")

lm <- function(r) 2 + 1 + r
p <- sapply(norm, lm)
hist(p, breaks=20, col=rgb(1,0,0,0.5), xlab="P = mA + b + Res (m=1, A=2, b=1)", ylab="Density", main="Distribution of P|m, A, b, Res")


#------------------------ Plotting Residuals
mysample = mydata[sample(nrow(mydata), 10), ] #randomly sample 10 rows from data

#scatter plot those 10 data points
plot(mysample$Sqft, mysample$Price, main="Price vs Sqft (with Residuals)", col=rgb(1,0,0,0.5),
     xlab="Sqft", ylab="Price", pch=19, cex=1.5)

#add graph of linear model
slope <- 100
intercept <- 100000
abline(a=intercept, b=slope, col=rgb(0,0,1,0.5), lwd=2)

#make predictions using model
model <- function(x) slope * x + intercept
predict <- sapply(mysample$Sqft, model)

#draw residuals
segments(mysample$Sqft, mysample$Price, mysample$Sqft, predict, col="green")

#label residuals
res <- 1:10  
for (i in c(1:10)){
  res[1] <- toString(i)
}

textxy(mysample$Sqft, mysample$Price, res, cex=0.7)

#------------------------ Polynomial regression overfitting
set.seed(100)
x <- seq(from=0, to=20, by=2)
y <- 10 + .2 * (x-10)^3
noise <- rnorm(length(x), mean=0, sd=100)
noisy_y <- y + noise

rainbowcols <- rainbow(7, s = 0.5)

plot(x, noisy_y, col='red', xlab='x', main='Observed data from 3rd model', pch=19)
y.pm <- lm(noisy_y ~ poly(x, 1))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[2], lwd=1)
y.pm <- lm(noisy_y ~ poly(x, 2))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[3], lwd=1)
y.pm <- lm(noisy_y ~ poly(x, 3))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[4], lwd=1)
y.pm <- lm(noisy_y ~ poly(x, 6))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[5], lwd=1)
y.pm <- lm(noisy_y ~ poly(x, 8))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[6], lwd=1)
y.pm <- lm(noisy_y ~ poly(x, 10))
lines(x, predict(y.pm, data.frame(x=x)), col=rainbowcols[7], lwd=1)


#------------------------ Linear regression overfitting
x <- c(0, 5, 6, 8, 10, 12, 15, 25)
y <- 10 + .2 * (x-10)^3

plot(x, y, col='red', xlab='x', main='Observed data from 3rd deg model', pch=19)
y.pm <- lm(y ~ poly(x, 1))
coeffs <- coefficients(y.pm)
lines(x, predict(y.pm, data.frame(x=x)), col=rgb(0,0,1,0.5), lwd=2)
text(2, 500, paste("m = ", round(coeffs[1], 1), "\nb = ", round(coeffs[2], 1)), pos = 4)

