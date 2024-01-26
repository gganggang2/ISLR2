library(ISLR2)
attach(Smarket)
pairs(Smarket, col=Direction)
dim(Smarket)
cor(Smarket)
names(Smarket)
cor(Smarket[,-9])

# Logistic Regression

?glm
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data = Smarket, family = binomial)
summary(Smarket)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
contrasts(Smarket$Direction)

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up"
glm.pred[1:10]
glm.probs[1:10]
summary(glm.pred)
summary(glm.probs)
table(glm.pred, Direction)
mean(glm.pred == Direction)

# Training data 만들기
train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
summary(train)
Direction.2005 <- Direction[!train]
Smarket.2005[1:10,]
dim(Smarket.2005)
glm.fits <- glm(
  Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
  data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fits <- glm(
  Direction ~ Lag1 + Lag2, 
  data = Smarket, family = binomial, subset = train)
predict(glm.fits, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), 
        type = "response")

# LDA
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, 
               subset = train)
lda.fit
coef(lda.fit)
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
summary(lda.pred)
lda.class <- lda.pred$class
lda.class
head(lda.pred)
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[,1] >= 0.5) #posterior prediction >= 0.5인 것의 합
sum(lda.pred$posterior[,1] < 0.5) # why [,1]? since it has 2 columns
sum(lda.pred$posterior[,1] >= 0.5203)

# QDA
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
summary(qda.class)
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# Naive Bayes
library(e1071)
nb.fit <- naiveBayes(Direction~Lag1+Lag2, data = Smarket, subset = train)
nb.fit
summary(nb.fit)
names(nb.fit)
mean(Lag1[train][Direction[train] == "Down"])
sd(Lag1[train][Direction[train] == "Down"])
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class, Direction.2005)
mean(nb.class == Direction.2005)
summary(nb.class)
nb.class
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw")
nb.preds[1:5, ]
?type

# KNN
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
train.X
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]
train.Direction

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86])
var(Caravan[,1])
var(standardized.X[,1]) #우왕 표준화 개쉽네

test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

glm.fits <- glm(Purchase ~., data = Caravan, family = binomial, subset = -test)
glm.probs <- predict(glm.fits, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000) # No를 10번 반복
glm.pred[glm.probs > 0.25] <- "Yes"
table(glm.pred, test.Y)
     
# Poission
attach(Bikeshare)
dim(Bikeshare)
mod.lm <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data = Bikeshare
)
summary(mod.lm)
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit, 
  data = Bikeshare
)
summary(mod.lm2)
all.equal(predict(mod.lm), predict(mod.lm2))
coef.months <- c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12]))
plot(coef.months, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "blue", pch = 19, type = "o")
#이렇게 하면 평균을 기준으로 비교할 수가 있음

axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
?axis

coef.hours <- c(coef(mod.lm2)[13:35],
                -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19, type = "o")

mod.pois <- glm(bikers ~ mnth + hr + workingday + temp + weathersit, 
                data = Bikeshare, family = poisson)
summary(mod.pois)

coef.months <- c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12]))
plot(coef.months, xlab = "Month", ylab = "Coefficient", xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.lm2)[13:35],
                -sum(coef(mod.lm2)[13:35]))
plot(coef.hours, xlab = "Hour", ylab = "Coefficient", col = "blue", pch = 19, type = "o")

plot(predict(mod.lm2), predict(mod.pois, type = "response"))
abline(0, 1, col = 3, lwd = 3)
