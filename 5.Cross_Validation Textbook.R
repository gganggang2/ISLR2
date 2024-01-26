set.seed(1)
train <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

# LOOCV
glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

cv.error <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
} #delta에 loocv값 두 개 나오는데 1번이 raw 2번이 조절된 값임
cv.error
degree <- 1:10
plot(degree, cv.error, type = "b", col = "red") # plot is x~y

# K-Fold CV
set.seed(17)
cv.error.10 <- rep(0,10)
for(i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
cv.error.10
lines(degree, cv.error.10, type = "b", col = "blue")

# Bootstrap
## First, create the function that you are interested in 
alpha.fn <- function (data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y))
}
names(Portfolio)
summary(Portfolio)
View(Portfolio)
alpha.fn(Portfolio, 1:100) # 1:100까지의 분산 공분산들이 계산되는 것

set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace=T)) # 계속 바뀔 것
?sample
boot(Portfolio, alpha.fn, R = 1000) # boot 돌린 것
?boot.ci
boot.ci(boot(Portfolio, alpha.fn, R = 1000), conf = 0.95) # 이게 맞을까?

# Bootstrap Accuracy test
boot.fn <- function(data, index) +
  coef(lm(mpg~horsepower, data = data, subset = index))
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T)) # 이걸 여러 번 하는 게 스킵됨
boot(Auto, boot.fn, 1000)
?boot
summary(lm(mpg~horsepower, data = Auto))$coef
# 결국에는 부트스트랩을 통해서 STDEV를 알고 싶은 것임. 원래는 STDEV를 구하려면 모종의 가정들이 필요하다. 근데 얘는 그런거 필요 없음!