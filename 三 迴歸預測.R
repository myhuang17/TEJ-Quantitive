#https://medium.com/tej-api-%E9%87%91%E8%9E%8D%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90/%E9%87%8F%E5%8C%96%E5%88%86%E6%9E%90-%E4%B8%89-%E9%A0%90%E6%B8%AC%E5%B8%82%E5%A0%B4-bde88352a011
library(quantmod)
library(tseries)
library(forecast)
library(ggplot2)
library(lubridate)
library(TTR)
library(zoo)
library(bsts)
data = getSymbols("2454.TW", auto.assign = FALSE, from="2008-01-01", to=Sys.Date())
rundata = data$`2454.TW.Adjusted`[complete.cases(data$`2454.TW.Adjusted`)]
#檢定-定態/非定態
orig = adf.test(rundata)
rundata = diff(log(rundata))
storder = adf.test(rundata[-1])
#迴歸-AR MODEL
model1 = arima(rundata[-1], c(3, 0, 0))
#羅吉斯迴歸(?)用在這裡感覺怪怪ㄉ
tat = as.numeric(sign(rundata[-1,]))
tat_l = tat[-1]
tat_l[length(tat)] = -1
tat_l[which(tat_l == 0)] = 0-1
tat = as.numeric(rundata[-1,])
runn = as.data.frame(cbind(tat, tat_l))
glm(tat_l~., data = runn)

#=================多餘的部分