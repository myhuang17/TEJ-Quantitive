#https://medium.com/tej-api-%E9%87%91%E8%9E%8D%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90/%E9%87%8F%E5%8C%96%E5%88%86%E6%9E%90-%E4%BA%8C-%E6%8A%80%E8%A1%93%E5%88%86%E6%9E%90%E7%B0%A1%E4%BB%8B%E8%88%87%E5%9B%9E%E6%B8%AC-14ae9bbb9c76
library(quantmod)
library(ggplot2)
library(lubridate)
library(TTR)
library(zoo)
#取得資料-2303聯電
data = getSymbols("2303.TW", auto.assign = FALSE, from="2008-01-01", to=Sys.Date())
data = data[complete.cases(data),]
chartSeries(data)
#MACD ===== https://www.twblogs.net/a/5b7fafc12b717767c6b10196
macd_data = MACD(data$`2303.TW.Adjusted`, nFast = 12, nSlow = 26, percent = F)
DIF = macd_data$macd
MACD = 2 * (macd_data$macd - macd_data$signal)
#EMA12 = filter(as.double(data$`2303.TW.Adjusted`)/12, rep(1, 12))
EMA12 = SMA(as.double(data$`2303.TW.Adjusted`), 12)
EMA26 = SMA(as.double(data$`2303.TW.Adjusted`), 26)
MACD_list = cbind(EMA12, EMA26, DIF, MACD)

#布林通道 ===== https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/bollinger-band.html
addBBands(n = 20, sd = 2)
BBands = function (price,n,sd){#布林(股票, 天數, 幾個標準差)
  mavg = SMA(price,n)
  sdev = rep(0,n)
  N = nrow(price)
  for (i in (n+1):N){
    sdev[i]= sd(price[(i-n+1):i])
  }
  up = mavg + sd*sdev
  dn = mavg - sd*sdev
  pctB = (price - dn)/(up - dn)
  output = cbind(dn, mavg, up, pctB)
  colnames(output) = c("dn", "mavg", "up", "pctB")#pctB可以看出是否超出上下界
  return(output)
}
bds = BBands(data$`2303.TW.Adjusted`, 20, 2)

#RSI指標 ===== https://www.rdocumentation.org/packages/TTR/versions/0.24.3/topics/RSI
RSI(data$`2303.TW.Adjusted`, n = 14)

#ATR ===== https://www.rdocumentation.org/packages/TTR/versions/0.24.3/topics/ATR
atr = ATR(data[,c(2, 3, 4)], n = 14)

#KD指標 ===== http://clover59.blogspot.com/2020/12/rkdj.html
stock_length = nrow(data)        # 數據長度
KDJ = matrix(NA, stock_length, 3) # 構建存放數據的矩陣
KDJ = as.data.frame(KDJ)          # 轉換為data.frame
colnames(KDJ) = c("K", "D", "J")  # 1-3列的名稱為K,D,J
KDJ[1:8, ] = 50                   # 前8天的K,D,J均設為50
high_max <- runMax(Hi(data), n=9) # 計算9日內最高價
low_min <- runMin(Lo(data), n=9)  # 計算9日內最低價
rsv <- (Cl(data) - low_min)/(high_max - low_min) * 100 #計算rvs
for(i in 9:stock_length) {
  KDJ[i, 1] <- 2/3 * KDJ[(i-1), 1] + 1/3 * rsv[i, ]  #計算K值
  KDJ[i, 2] <- 2/3 * KDJ[(i-1), 2] + 1/3 * KDJ[i, 1] #計算D值
  KDJ[i, 3] <- 3 * KDJ[i, 1] - 2 * KDJ[i, 2]         #計算J值
}

#回測應用 ===== 
#交易策略設計：
#買入信號：
#MACD上升 且 K值>70 且 當日K值>前日K值
#賣出信號：
#當日低點<前日收盤 - 前日 ATR
decision = NULL
MACD[is.na(MACD)] = 0
MACD = as.vector(MACD)
KDJ_k = as.vector(KDJ$K)
close = as.vector(data$`2303.TW.Close`)
aatr = as.vector(atr$atr)
low = as.vector(data$`2303.TW.Low`)
for(x in 2:length(data[,1])){
  if((MACD[x]-MACD[x-1] > 0) & (KDJ_k[x]>70) & {KDJ_k[x] > KDJ_k[x-1]}){
    decision[x] = "BUY"
  }
  else if(low[x] < (close[x-1] - aatr[i])){
    decision[x] = "SELL"
  }
  else{
    decision[x] = "NOTHING"
  }
}
result = cbind(data, decision)
