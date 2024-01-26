library(ISLR2)
library(leaps)
install.packages("leaps")
library(leaps)
View(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters <- na.omit(Hitters)
dim(Hitters)

regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)
regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
reg.summary
View(reg.summary)
names(reg.summary)
reg.summary$rsq
reg.summary$rss

plot(1:19, reg.summary$bic, typ = "b", col = "red")
par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", 
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")
?par
par(mfrow = c(1,1)) # 2행 1열의 도화지 생성

which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "red", cex = 2, pch = 10)
?pch
# rss, cp, bic는 작을 수록 좋은거니까 which.min() 을 사용해야겠지? 참고로 index 뽑아주는거임

plot(regfit.full, scale = "adjr2")
reg.summary$which
coef(regfit.full, 6) # 이게 왜 되는 거임? 원리좀

# Forward & Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7) 
coef(regfit.bwd, 7) # 이 3가지가 묘하게 다른 걸 확인할 수 있답니다

set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = TRUE)
test <- (!train)
regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19)

test.mat <- model.matrix(Salary~., data = Hitters[test,])
View(test.mat)
val.errors <- rep(NA,19) # 이 vector에다가 값을 넣을거임
for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
                   val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 7)
coef(regfit.best, 4)

# predict regsubsets 함수 생성기
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi # pipe기호 하나 잘 못써서 작살났네
}

regfit.best <- regsubsets(Salary~., data=Hitters, nvmax = 19)
coef(regfit.best, 7)

k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19))) #의미?

for(j in 1:k){
  best.fit <- regsubsets(Salary~.,
                         data = Hitters[folds != j,], #fold가 j가 아닌 data만
                         nvmax = 19) # 여기까지는 fit만 함 예측 아직 안했음
  for (i in 1:19){
    pred <- predict.regsubsets(best.fit, Hitters[folds == j,], id = i) #여기서 predict함
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
?apply
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")
reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 10) #이건 또 잘 뽑는데?

# Ridge Regression and LASSO
x <- model.matrix(Salary~., Hitters)[,-1] #1번째 컬럼 제외
y <- Hitters$Salary
?model.matrix

install.packages("glmnet")
library(glmnet)
grid <- 10^seq(10,-2,length=100)
grid
seq(10, -2, length = 100)
ridge.mod <- glmnet(x,y,alpha = 0, lambda = grid)
dim(coef(ridge.mod))
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
coef(ridge.mod)[,60]
round(sqrt(sum(coef(ridge.mod)[-1,60]^2)), 1) # get bigger
predict(ridge.mod, s=50, type="coefficients")[1:20,]

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train) # ! 안 쓰고 - 쓰는 이유는 matrix여서 그런 것 같음
y.test <- y[test]
length(y.test)
length(train)

ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred - y.test)^2)
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T, x = x[train,], y = y[train]) #WHY?
mean((ridge.pred - y.test)^2)
lm(y~x, subset = train)
predict(ridge.mod, s=0, exact=T, type = "coefficients", x = x[train,], y = y[train])[1:20, ]

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
names(cv.out)
bestlam
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
ridge.pred
round(mean((ridge.pred-y.test)^2), 0)
out <- glmnet(x,y,alpha=0)
predict(out, type ="coefficient", s = bestlam)[1:20,]

## LASSO
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]

# PCR and PLS Regression
install.packages("pls")
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred <- predict(pcr.fit, x[test,], ncomp=5)
mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 5)
summary(pcr.fit)

##PLS
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pls.pred <- predict(pls.fit, x[test,], ncomp=1)
mean((pls.pred-y.test)^2)
pls.fit <- plsr(Salary~., data = Hitters, scale = TRUE, ncomp = 1)
# 그래서 ncomp = 1일 때 제일 작다는 걸 어떻게 알 수 있지 MSE 어케구함?
