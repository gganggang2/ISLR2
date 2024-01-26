library(ISLR2)
require(ISLR2)
names(Smarket)
Smarket <- Smarket
summary(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Smarket,family=binomial)
summary(glm.fit)
# Since the family is binomial, we could call this Logistic Regression
glm.probs=predict(glm.fit, type="response")
glm.probs[1:5]
glm.pred=ifelse(glm.probs>0.5, "Up", "Down")
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred==Direction)

# Making Traning Set
train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
              data=Smarket, family=binomial, subset = train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down") # 자동 넘버링?
Direction.2005 = Smarket$Direction[!train]
Direction.2005
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)

# Fit smaller model for removing overfitting
glm.fit=glm(Direction~Lag1+Lag2,
            data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
summary(glm.fit)

require(MASS)

## Linear Discriminant Analysis
lda.fit=lda(Direction~Lag1+Lag2, data = Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year==2005)
Smarket.2005
lda.pred=predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
lda.pred
summary(lda.pred)
data.frame(lda.pred)[1:5,] # list형인 경우
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-Nearest neighbors
library(class)
attach(Smarket)
ls()
objects(2)
Xlag=cbind(Lag1,Lag2)
Xlag[1:5,]
train=Year<2005
knn.pred=knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])
