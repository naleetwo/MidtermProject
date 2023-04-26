year_all <- read.csv("C:/ljk9707/PythonWork/Analysis/data/MidtermProject/year_long_concat/year_all.csv", encoding = "utf-8")
year_all <- year_all[-1]
View(year_all)

par(mfrow=c(3, 2))
plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.매매금액.만원.)
plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.전월세금액.만원.)
plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.매매금액.만원.)
plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.전월세금액.만원.)
plot(year_all$학교수, year_all$X1m.2.전용면적당.매매금액.만원.)
plot(year_all$학교수, year_all$X1m.2.전용면적당.전월세금액.만원.)


trade_coffee <- lm(X1m.2.전용면적당.매매금액.만원. ~ CoffeeShop , data = year_all)
summary(trade_coffee) #0.3497
shapiro.test(resid(trade_coffee)) # 0.0001072 정규분포가 아니다

rent_coffee <- lm(X1m.2.전용면적당.전월세금액.만원. ~ CoffeeShop , data = year_all)
summary(rent_coffee) #0.377
shapiro.test(resid(rent_coffee)) # 0.00259 정규분포가 아니다

trade_Convenience <- lm(X1m.2.전용면적당.매매금액.만원. ~ ConvenienceStore , data = year_all)
summary(trade_Convenience) #0.2951
shapiro.test(resid(trade_Convenience)) # 1.126e-05 정규분포가 아니다

rent_Convenience <- lm(X1m.2.전용면적당.전월세금액.만원. ~ ConvenienceStore , data = year_all)
summary(rent_Convenience) #0.2524
shapiro.test(resid(rent_Convenience)) # 0.005047 정규분포가 아니다

trade_school <- lm(X1m.2.전용면적당.매매금액.만원. ~ 학교수 , data = year_all)
summary(trade_school) # 0.01994
shapiro.test(resid(trade_school)) # 2.344e-05 정규분포가 아니다

rent_school <- lm(X1m.2.전용면적당.전월세금액.만원. ~ 학교수 , data = year_all)
summary(rent_school) #0.002128
shapiro.test(resid(rent_school)) # 0.01085 정규분포가 아니다


cor.test(year_all$CoffeeShop, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.5561565
cor.test(year_all$CoffeeShop, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #0.5566785
cor.test(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.3658672
cor.test(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #0.2884195
cor.test(year_all$학교수, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.046082
cor.test(year_all$학교수, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #-0.0271571 

par(mfrow=c(3, 2))
plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.매매금액.만원.)
abline(trade_coffee, col="blue")
plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.전월세금액.만원.)
abline(rent_coffee, col="blue")
plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.매매금액.만원.)
abline(trade_Convenience, col="blue")
plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.전월세금액.만원.)
abline(rent_Convenience, col="blue")
plot(year_all$학교수, year_all$X1m.2.전용면적당.매매금액.만원.)
abline(trade_school, col="blue")
plot(year_all$학교수, year_all$X1m.2.전용면적당.전월세금액.만원.)
abline(rent_school, col="blue")

out <- lm(X1m.2.전용면적당.매매금액.만원. ~ ., data=year_all)
summary(out)
