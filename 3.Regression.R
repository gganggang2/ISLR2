library(ISLR2)
names(Boston)
?Boston
plot(medv~lstat, data=Boston)
fit1 = lm(medv~lstat, data=Boston)

fit1
summary(fit1)
abline(fit1, col="red")
?abline
names(fit1)
confint(fit1)
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(fit1, data.frame(lstat=c(5,10,15)), interval = "prediction")

fit2 = lm(medv~lstat+age, data=Boston)
summary(fit2)

fit3 = lm(medv~., Boston)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)

fit4 = update(fit3, ~.-age-indus)
summary(fit4)

fit5 = lm(medv~lstat*age, Boston)
summary(fit5)

fit6 = lm(medv~lstat + I(lstat^2), Boston); summary(fit6)
attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat, fitted(fit6), col="red", pch = 20)
fit7 = lm(medv~poly(lstat,4))
points(lstat, fitted(fit7), col='blue', pch = 20)
plot(1:20, 1:20, pch=1:20, cex=2)

names(Carseats)
summary(Carseats)
Fit1 = lm(Sales~.+Income:Advertising+Age:Price, Carseats)
summary(Fit1)
contrasts(Carseats$ShelveLoc) #Dummy Coding

regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit, col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit, col="red")
}
regplot(Price, Sales, xlab="Price", ylab="Sales", col="blue", pch=20)

fit1$coefficients
coef(fit1)
confint(fit1)
confint(fit1, level=0.99)
plot(residuals(fit1))
plot(hatvalues(fit1))
which.max(hatvalues(fit1))
summary(fit4)$sigma
summary(fit4)$r.sq

attach(Boston)
lm.fit5 <- lm(medv ~ poly(lstat,5, raw=TRUE))
summary(lm.fit5)

