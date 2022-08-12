#https://medium.com/tej-api-%E9%87%91%E8%9E%8D%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90/%E9%87%8F%E5%8C%96%E5%88%86%E6%9E%90-%E4%B8%83-%E6%95%88%E7%8E%87%E5%89%8D%E7%B7%A3%E6%9B%B2%E7%B7%9A-cfbe972274
library(quantmod)
library(tseries)
library(fPortfolio)
library(TTR)
library(zoo)
# TIME
#取得資料
target = c("2330.TW","2603.TW","2912.TW")
getSymbols(target, from="2015-01-01", to=Sys.Date(), adjust=TRUE)
data = cbind(`2330.TW`$`2330.TW.Close`, `2603.TW`$`2603.TW.Close`, `2912.TW`$`2912.TW.Close`)
data = data[complete.cases(data),]
ret = diff(log(data)) * 100
ret = ret[complete.cases(ret), ]

dim(na.omit(ret))#dim獲取或設置指定矩陣;na.omit返回刪除NA後的向量RET
colnames(RET)=target

#效率曲線-tangency
cons = c('LongOnly')
spect = portfolioSpec()
setNFrontierPoints(spect) = 250
setSolver(spect) = "solveRquadprog"
frontier = portfolioFrontier(as.timeSeries(ret))
tailoredFrontierPlot(frontier)#前緣曲線圖
weightsPlot(frontier)#比例圖
weightedReturnsPlot(frontier)
x = equalWeightsPoints(frontier)#EWP

#1. Least Risk
result = efficientPortfolio(as.timeSeries(ret))#riskless point
tangencyPortfolio(as.timeSeries(ret))#tangency point

#2. QP1
spec1 = portfolioSpec()
setTargetReturn(spec1) = mean(apply(ret, 1, mean))
qp1 = efficientPortfolio(as.timeSeries(ret), spec = spec1)#****
weightedReturnsPlot(frontier)
