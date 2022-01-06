# interaction term
library(MASS)
library(ISLR)

attach(Boston)
head(Boston)
# lstat :소득분위, medv : 집값의 중앙값, 두 변수는 반비례관계 

fit <- lm(medv ~ lstat+age)
summary(fit)

par(mfrow = c(2,2))
plot(fit)
plot(lstat,medv)
plot(age,medv)
plot(age,lstat)

fit2 = lm(medv ~ lstat*age)
summary(fit2)
fit3 <- lm(medv ~ lstat+age+lstat:age)
summary(fit3) # 위와 동일 
#lm(medv ~ lstat+lstat:age), 교호작용항만 추가하기 
detach(Boston)

# nonlinear 
library(MASS)
library(ISLR)
attach(Boston)

lstat
fit <- lm(medv~lstat)
summary(fit)

fit = lm(medv~lstat+lstat^2) ###이렇게 하면 제곱항 안나옴,, lstat을 같은거 취급 
summary(fit)

fit2 = lm(medv~lstat+I(lstat^2)) ###이렇게 해야함
summary(fit2)

anova(fit, fit2) # 2개의 모형 비교 
# F통계량의 값이 fit2의 lstat의 제곱항의 t통계량의 제곱과 비슷 : 11.63^2 = 135.2 
# 두 모형의 차이를 판단하는 것이므로 lstst^2의 차이를 판단하는 코드다 

par(mfrow = c(2,2))
plot(fit) 
plot(fit2)
# fit보다 fit2가 residual vs fitted 그림을 보면 선형성이 없어보이므로
# fit2가 더 적합하다는 근거가 될 수 있다. 

###이렇게 해도됨
lstat2 = lstat^2
fit3 = lm(medv~lstat+lstat2)
par(mfrow = c(2,2))
plot(fit2)


###polynomial regression
fit4 <- lm(medv~lstat+I(lstat^2)+I(lstat^3)) ##이렇게 하면 힘듦
fit5 <- lm(medv ~ poly(lstat,3))

summary(fit4)
summary(fit5)
# 2개의 모델 자체는 동일 
###testing 결과(F통계량, 결정계수 등)는 같으나 계수가 다르다. 
# poly는 직교화를 하는듯(설명변수 값들을 변경한다음 fitting)

fit5 <- lm(medv ~ poly(lstat,5))
summary(fit5)









