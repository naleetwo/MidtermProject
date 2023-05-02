library(car)
library(ggplot2)
library(dplyr)
library(leaps)
getwd()
setwd("C:\\Users\\owner\\team project")

#### 경제적요인 ####

data<-read.csv("17년치 분기별 요인(원계열).csv",encoding = "UTF-8")
View(data)
data
attach(data)

# 종속변수(면적당거래가격)과 독립변수들간 상관계수 분석

cor(data[, c("면적당거래가격","물가지수","기준금리","실업율","고용률","GDP","유동성비율","인구이동률")])


# 상관관계가 낮은 요인을 제거하고 다중회귀분석

fit<-lm(면적당거래가격~물가지수+기준금리+GDP+유동성비율+인구이동률+고용률,data=data)

summary(fit)
shapiro.test(resid(fit))
vif(fit)

#물가지수   기준금리        GDP 유동성비율 인구이동률 
#25.866407   2.506114  24.120971   6.261985   6.515765 

# 최적의 값    

reg <- lm(면적당거래가격~물가지수+기준금리+GDP+유동성비율+인구이동률+고용률,data=data)
reduce.reg<-step(reg, direction="backward")

leap <- regsubsets(면적당거래가격~물가지수+기준금리+GDP+유동성비율+인구이동률+고용률,data=data, nbest=5)
leap
par(mfrow=c(1,1))
plot(leap, scale="adjr2") 

#보정
powerTransform(면적당거래가격)

fit1<-lm(면적당거래가격^(-1)~물가지수+기준금리+GDP+유동성비율+인구이동률+고용률,data=data)

summary(fit1)
vif(fit1)
shapiro.test(resid(fit1))

fit<-lm(면적당거래가격^(-1)~물가지수+기준금리+GDP+유동성비율+인구이동률+고용률,data=data)

# 물가지수   기준금리        GDP 유동성비율 인구이동률 
#25.866407   2.506114  24.120971   6.261985   6.515765 

reg <- lm(면적당거래가격^(-1)~기준금리+유동성비율+인구이동률+고용률,data=data)
reduce.reg<-step(reg, direction="backward")

leap <- regsubsets(면적당거래가격^(-1)~기준금리+유동성비율+인구이동률+고용률,data=data, nbest=6)
leap
par(mfrow=c(1,1))
plot(leap, scale="adjr2") 

fit<-lm(면적당거래가격^(-1) ~ 기준금리 + 인구이동률,data=data)

summary(fit)
vif(fit)
shapiro.test(resid(fit))
ncvTest(fit)


fit<-lm(면적당거래가격^(-1) ~ log(기준금리) + 인구이동률,data=data)

summary(fit)
vif(fit)
shapiro.test(resid(fit))
ncvTest(fit)






