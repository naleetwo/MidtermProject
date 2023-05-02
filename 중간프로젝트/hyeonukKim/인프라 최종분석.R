# 최종분석법


library(car)
library(ggplot2)
library(dplyr)
library(leaps)

setwd("C:\\Users\\owner\\team project")
data<-read.csv("인프라모든요인_자치구변환추가.csv",encoding = "UTF-8")

View(data)

attach(data)

#### 매매가와 인프라 사이의 영향이 있는 요인조사 ####

fit<-lm(매매가~범죄건수+정류장수+편의점수+카페수+학교수+병원수
           +학원수+대학교수)
summary(fit)
vif(fit)
shapiro.test(resid(fit)) # 주요 통계량

reg <- lm(매매가~대학교수+편의점수+카페수+범죄건수+병원수+학교수+학원수+정류장수)
reduce.reg<-step(reg, direction="backward") # AIC 비교

powerTransform(매매가) # 변환 최적화

fit<-lm(매매가^(-0.25) ~ 대학교수 + 편의점수 + 카페수 + 범죄건수 + 병원수 + 학교수 + 학원수 + 정류장수)
summary(fit)
vif(fit)
shapiro.test(resid(fit)) # 보정후 전체요인 다중회귀분석

reg <- lm(매매가^(-.25)~대학교수+편의점수+카페수+범죄건수+병원수+학교수+학원수+정류장수)
reduce.reg<-step(reg, direction="backward") # 보정후 AIC 비교

fit<-lm(매매가^(-0.25) ~ 카페수 + 학교수 + 학원수)
summary(fit)
vif(fit)
shapiro.test(resid(fit)) # 보정후 AIC가 안정적인 요인 다중회귀분석
                         

#Step:  AIC=-871.06 매매가^(-0.25) ~ 카페수 + 학교수 + 학원수
#Step:  AIC=-870.18 매매가^(-0.25) ~ 카페수 + 학교수 + 학원수 + 정류장수
#Step:  AIC=-865.32 매매가^(-0.25) ~ 대학교수 + 편의점수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수
#Step:  AIC=-868.95 매매가^(-0.25) ~ 카페수 + 병원수 + 학교수 + 학원수 + 정류장수
#Step:  AIC=-867.23 매매가^(-0.25) ~ 대학교수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수


fit1<-lm(매매가^(-0.25) ~ 카페수 + 학교수 + 학원수)
summary(fit1)
vif(fit1)
shapiro.test(resid(fit1))

fit2<-lm(매매가^(-0.25) ~ 카페수 + 학교수 + 학원수 + 정류장수)
summary(fit2)
vif(fit2)
shapiro.test(resid(fit2))

fit3<-lm(매매가^(-0.25) ~ 대학교수 + 편의점수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
summary(fit3)
vif(fit3)
shapiro.test(resid(fit3))

fit4<-lm(매매가^(-0.25) ~ 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
summary(fit4)
vif(fit4)
shapiro.test(resid(fit4))

fit5<-lm(매매가^(-0.25) ~ 대학교수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
summary(fit5)
vif(fit5)
shapiro.test(resid(fit5)) # AIC가 낮은순 5개의 조합에 대한 통계량분석

fit1<-lm(매매가^(-0.25) ~ 카페수 + 학교수 + 학원수)
fit2<-lm(매매가^(-0.25) ~ 카페수 + 학교수 + 학원수 + 정류장수)
fit3<-lm(매매가^(-0.25) ~ 대학교수 + 편의점수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
fit4<-lm(매매가^(-0.25) ~ 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
fit5<-lm(매매가^(-0.25) ~ 대학교수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)

shapiro.test(resid(fit1))
shapiro.test(resid(fit2))
shapiro.test(resid(fit3))
shapiro.test(resid(fit4))
shapiro.test(resid(fit5))

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)


vif(fit1)
vif(fit2)
vif(fit3)
vif(fit4)
vif(fit5)

fit2<-lm(매매가^(-0.25) ~ log(카페수) + 학교수 + 학원수 + 정류장수)
summary(fit2)
vif(fit2)
shapiro.test(resid(fit2))









#### 






#### 전월세와 인프라 사이의 영향이 있는 요인조사 #####

fit<-lm(전월세가~범죄건수+정류장수+편의점수+카페수+학교수+병원수
                   +학원수+대학교수)
summary(fit)
vif(fit)
shapiro.test(resid(fit)) #전체요인 분석

reg <- lm(전월세가~대학교수+편의점수+카페수+범죄건수+병원수+학교수+학원수+정류장수)
reduce.reg<-step(reg, direction="backward") 
# AIC 비교

powerTransform(전월세가)  #종속변수 보정 

fit<-lm(전월세가^(0.05)~범죄건수+정류장수+편의점수+카페수+학교수+병원수
            +학원수+대학교수)
summary(fit)
vif(fit)
shapiro.test(resid(fit))

reg <- lm(전월세가^(0.05)~대학교수+편의점수+카페수+범죄건수+병원수+학교수+학원수+정류장수)
reduce.reg<-step(reg, direction="backward") 

# AIC 비교


fit1<-lm(전월세가^(0.05) ~ 대학교수 + 편의점수 + 카페수 + 범죄건수 + 병원수 + 학교수 + 학원수 + 정류장수)
fit2<-lm(전월세가^(0.05) ~ 대학교수 + 편의점수 + 카페수 + 병원수 + 학교수 + 학원수 + 정류장수)
fit3<-lm(전월세가^(0.05) ~ 대학교수 + 편의점수 + 카페수 + 학교수 + 학원수 + 정류장수)
fit4<-lm(전월세가^(0.05) ~ 편의점수 + 카페수 + 학교수 + 학원수 + 정류장수)
fit5<-lm(전월세가^(0.05) ~ 편의점수 + 카페수 + 학교수 + 학원수) 
#최적의 조합에 대한 다중회귀분석

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)

vif(fit1)
vif(fit2)
vif(fit3)
vif(fit4)
vif(fit5)

shapiro.test(resid(fit1))
shapiro.test(resid(fit2))
shapiro.test(resid(fit3))
shapiro.test(resid(fit4))
shapiro.test(resid(fit5))

fit1<-lm(전월세가^(0.05) ~ 편의점수 + log(카페수) + 학교수 + 학원수 + 정류장수)



fit<-lm(전월세가^(0.05) ~ 편의점수 + log(카페수) + 학교수 + 학원수 + 정류장수 + 대학교수)
summary(fit)
shapiro.test(resid(fit))
vif(fit)



summary(fit2)

vif(fit1)
vif(fit2)


shapiro.test(resid(fit2))
