year_all <- read.csv("C:/ljk9707/PythonWork/Analysis/data/MidtermProject/year_long_concat/year_all.csv", encoding = "utf-8")
year_all <- year_all[-1]
View(year_all)

shapiro.test(year_all$CoffeeShop) # 1.088e-10 정규분포가 아니다
cor.test(year_all$CoffeeShop, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.5646974

shapiro.test(year_all$CoffeeShop) # 1.088e-10 정규분포가 아니다
cor.test(year_all$CoffeeShop, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #0.548977

shapiro.test(year_all$ConvenienceStore) # 2.078e-11 정규분포가 아니다
cor.test(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.3539746

shapiro.test(year_all$ConvenienceStore) # 2.078e-11 정규분포가 아니다
cor.test(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #0.2638472

shapiro.test(year_all$학교수) # 1.574e-07 정규분포가 아니다
cor.test(year_all$학교수, year_all$X1m.2.전용면적당.매매금액.만원., method="spearman") #0.04527286

shapiro.test(year_all$학교수) # 1.574e-07 정규분포가 아니다
cor.test(year_all$학교수, year_all$X1m.2.전용면적당.전월세금액.만원., method="spearman") #-0.02320714 

par(mfcol=c(2, 3))
plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.매매금액.만원.)
trade_coffee <- lm(X1m.2.전용면적당.매매금액.만원.~CoffeeShop, data=year_all)
abline(trade_coffee, col="blue")

plot(year_all$CoffeeShop, year_all$X1m.2.전용면적당.전월세금액.만원.)
rent_coffee <- lm(X1m.2.전용면적당.전월세금액.만원.~CoffeeShop, data=year_all)
abline(rent_coffee, col="blue")

plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.매매금액.만원.)
trade_Convenience <- lm(X1m.2.전용면적당.매매금액.만원.~ConvenienceStore, data=year_all)
abline(trade_Convenience, col="blue")

plot(year_all$ConvenienceStore, year_all$X1m.2.전용면적당.전월세금액.만원.)
rent_Convenience <- lm(X1m.2.전용면적당.전월세금액.만원.~ConvenienceStore, data=year_all)
abline(rent_Convenience, col="blue")

plot(year_all$학교수, year_all$X1m.2.전용면적당.매매금액.만원.)
trade_school <- lm(X1m.2.전용면적당.매매금액.만원.~학교수, data=year_all)
abline(trade_school, col="blue")

plot(year_all$학교수, year_all$X1m.2.전용면적당.전월세금액.만원.)
rent_school <- lm(X1m.2.전용면적당.전월세금액.만원.~학교수, data=year_all)
abline(rent_school, col="blue")

?par
?plot
