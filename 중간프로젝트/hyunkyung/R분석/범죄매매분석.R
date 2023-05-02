#### 연습 ####
Sys.getlocale()
Sys.setlocale("LC_ALL","C")
data <- read.csv('C:/acorn/midproject/crimerate/final_crime_gu_year.csv', encoding='utf-8')
Sys.setlocale("LC_ALL","Korean")
View(data)

names(data)

x <- data$crime_num
y <- data$X1m.2.전용면적당.거래금액.만원.

cor(x, y, method='pearson')
cor(x, y, method = 'spearman')
cor.test(x, y, method = 'pearson')

df <- read.csv('C:/acorn/midproject/R분석/R2018범죄가격.csv')
View(df)

x2 <- df$X2018y2 <- df$X1m.2.전용면적당.거래금액.만원.

cor(x2, y2, method='pearson')
cor(x2, y2, method='spearman')
cor.test(x2, y2, method = 'pearson')


##### 인프라 모든 요인 #####

library(car)
library(dplyr)
library(ggplot2)
library(leaps)

df <- read.csv('C:/acorn/midproject/analysis/인프라모든요인.csv', fileEncoding='utf-8')
View(df)

df_buy <- as.data.frame(df[, c(4:12)])
View(df_buy)

df_rent <- as.data.frame(df[, c(4:11, 13)])
View(df_rent)

fit <- lm(매매가 ~ ., data=df_buy) # . → 모든 변수를 독립변수로 넣겠다.
summary(fit)
# Adjusted R-squared:  0.4436

vif(fit)

par(mfrow=c(2, 2))
plot(fit)

shapiro.test(resid(fit))

powerTransform(df_buy$매매가)
summary(powerTransform(df_buy$매매가))

fit <- lm(log(매매가) ~ ., data=df_buy)
summary(fit)

boxTidwell(매매가~카페수+학교수+학원수, data=df_buy)

# forward
min.model <- lm(매매가~1, data=df_buy)
fwd.model <- step(min.model, direction = 'forward', 
                  scope = (매매가 ~ 범죄건수 + 정류장수 + 편의점수 + 카페수 + 학교수 + 병원수 + 학원수 + 대학교수))

# all subset regression
leap <- regsubsets(매매가 ~ 범죄건수+정류장수+편의점수+카페수+학교수+병원수+학원수+대학교수, data=df_buy, nbest=8)
leap

par(mfrow=c(1, 1))
plot(leap, scale = 'adjr2')

reg2 <- lm(매매가~카페수+학교수+학원수, data=df_buy)
summary(reg2) # 학교수 음수

reg3 <- lm(매매가~카페수+학원수, data=df_buy)
summary(reg3) # Adjusted R-squared:  0.4152

# 정규성
shapiro.test(resid(reg3)) # 정규분포 아님
powerTransform(df_buy$매매가) # -0.2713583
summary(powerTransform(df_buy$매매가))

reg4 <- lm(매매가^(-0.27)~카페수+학원수, data=df_buy)
summary(reg4) # Adjusted R-squared:  0.326
shapiro.test(resid(reg4))

# 다중 공선성
vif(reg4)

# 등분산성
ncvTest(reg4)

spreadLevelPlot(reg4)

reg5 <- lm((매매가^(-0.27))^(-0.24)~카페수+학원수, data=df_buy)
summary(reg5) # Adjusted R-squared:   0.35 
ncvTest(reg5)

### 4월 27일
year_all = read.csv('C:/acorn/midproject/analysis/인프라모든요인.csv', fileEncoding = 'utf-8')
View(year_all)

year_all <- year_all[-1]

shapiro.test(year_all$범죄건수) # 2.387e-10 정규분포가 아니다
cor.test(year_all$범죄건수, year_all$매매가, method="spearman") # 0.3787686
cor.test(year_all$범죄건수, year_all$전월세가, method="spearman") # 0.3973608

shapiro.test(year_all$정류장수) # 0.3077 정규분포가 맞다
cor.test(year_all$정류장수, year_all$매매가, method="pearson") # 0.1359168
cor.test(year_all$정류장수, year_all$전월세가, method="pearson") # 0.07964421

shapiro.test(year_all$병원수) # 8.553e-15 정규분포가 아니다
cor.test(year_all$병원수, year_all$매매가, method="spearman") # 0.3705981
cor.test(year_all$병원수, year_all$전월세가, method="spearman") # 0.3395391

shapiro.test(year_all$학원수) # 3.062e-12 정규분포가 아니다
cor.test(year_all$학원수, year_all$매매가, method="spearman") # 0.3118478
cor.test(year_all$학원수, year_all$전월세가, method="spearman") # 0.305824

shapiro.test(year_all$대학교수) # 2.042e-09 정규분포가 아니다
cor.test(year_all$대학교수, year_all$매매가, method="spearman") # -0.07959212
cor.test(year_all$대학교수, year_all$전월세가, method="spearman") # -0.04760734

par(mfcol=c(2, 5))

plot(year_all$범죄건수, year_all$매매가)
trade_crime <- lm(매매가~범죄건수, data=year_all)
abline(trade_crime, col="blue")

plot(year_all$범죄건수, year_all$전월세가)
rent_crime <- lm(전월세가~범죄건수, data=year_all)
abline(rent_crime, col="blue")

plot(year_all$정류장수, year_all$매매가)
trade_bus <- lm(매매가~정류장수, data=year_all)
abline(trade_bus, col="blue")

plot(year_all$정류장수, year_all$전월세가)
rent_bus <- lm(전월세가~정류장수, data=year_all)
abline(rent_bus, col="blue")

plot(year_all$병원수, year_all$매매가)
trade_hos <- lm(매매가~병원수, data=year_all)
abline(trade_hos, col="blue")

plot(year_all$병원수, year_all$전월세가)
rent_hos <- lm(전월세가~병원수, data=year_all)
abline(rent_hos, col="blue")

plot(year_all$학원수, year_all$매매가)
trade_aca <- lm(매매가~학원수, data=year_all)
abline(trade_aca, col="blue")

plot(year_all$학원수, year_all$전월세가)
rent_aca <- lm(전월세가~학원수, data=year_all)
abline(rent_aca, col="blue")

plot(year_all$대학교수, year_all$매매가)
trade_uni <- lm(매매가~대학교수, data=year_all)
abline(trade_uni, col="blue")

plot(year_all$대학교수, year_all$전월세가)
rent_uni <- lm(전월세가~대학교수, data=year_all)
abline(rent_uni, col="blue")
