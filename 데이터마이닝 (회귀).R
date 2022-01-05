데이터마이닝 회귀

#install.packages("ISLR")
library(ISLR)
library(MASS)
attach(Boston)

head(Boston) ###보스턴 집값에 대한 자료
# lstat = 저산층 비율 , medv = 집값의 중앙값, 2개가 반비례할것이다 

pairs(Boston)

lm.fit <- lm(medv~lstat)
lm.fit ###coefficient만 나옴

summary(lm.fit) ###모든 계수가 유의하다.(0이 아니다)
24.53**2 ###lstat의 t-value의 제곱값.F통계량과 같다.

dim(Boston)
class(lm.fit) ###lm이라는 객체
names(lm.fit) # 객체 안에 속성?들 볼 수 있다 
lm.fit$coefficients ##계수
coef(lm.fit) ###계수
lm.fit$residuals ###잔차
confint(lm.fit) ##95% 신뢰구간

predict(lm.fit, data.frame(lstat = c(5,10,15)),interval = "confidence") 
# 5,10,15을 x로 넣었을 때 예측값 출력 
###데이터를 하나주고 예측값을 출력.하한값과 상한값도 출력.(신뢰대)
predict(lm.fit, data.frame(lstat = c(5,10,15)),interval = 'prediction') 
###하한, 상한값의 차이가 크다.(예측대)

plot(lstat,medv, pch=20, col= "blue")
abline(lm.fit,lwd = 3 , col = 'red')

par(mfrow = c(2,2))
plot(lm.fit) ###qqplot을보면 꼬리가 두껍다는걸 알수 있다


setwd(readClipboard())

library(MASS)
library(ISLR)
attach(Boston)
head(Boston)
dim(Boston)


str(Boston)


fit = lm(medv ~lstat+age)
summary(fit)
aov(fit)

(23243.914 + 304.253)/ (23243.914 + 304.253 + 19168.129)
###lstat(저소득층0)이 증가하면 medv(집값)이 떨어진다
### age가 증가하면 집값이 증가한다.

fit = lm(medv ~.,Boston)
summary(fit)

fit1 = update(fit,~.-age) ##후진제거법
summary(fit1)


fit2 = update(fit1,~.-indus)
summary(fit2)









