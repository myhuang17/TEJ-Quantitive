#https://medium.com/tej-api-%E9%87%91%E8%9E%8D%E8%B3%87%E6%96%99%E5%88%86%E6%9E%90/%E9%87%8F%E5%8C%96%E5%88%86%E6%9E%90-%E4%B8%80-%E5%AE%9A%E6%9C%9F%E5%AE%9A%E9%A1%8D%E8%88%87%E8%A4%87%E5%88%A9%E7%9A%84%E6%95%88%E6%9E%9C-8787f3851273
library(quantmod)
library(ggplot2)
library(lubridate)
library(PerformanceAnalytics)
#取得資料-0050
data = getSymbols("0050.TW", auto.assign = FALSE, from="2008-01-01", to=Sys.Date())
mth = endpoints(data, on="month")#取得各月尾數在第幾筆
mdata = rbind(data[mth,])

#計算每期相關資訊
month = seq(as.Date("2008-02-01"),length=length(mdata[,1]),by="months")-1
monthly = as.integer(10000/mdata$`0050.TW.Adjusted`)#buy 10000 for each month
accmulate_num = cumsum(monthly)#累積的股數
accu_money = monthly * mdata$`0050.TW.Adjusted`
accu_money = as.double(cumsum(accu_money))
acc_value = as.double(accmulate * mdata$`0050.TW.Adjusted`)#累積價值 // as.double
acc_rate = acc_value[length(acc_value)] /sum(monthly*mdata$`0050.TW.Adjusted`) - 1#累積報酬率 // as.double
newdata = cbind(monthly, accmulate_num, accu_money, mdata$`0050.TW.Adjusted`, acc_value)# 
newdata = as.data.frame(newdata)

#繪圖
#投入資金與報酬
ggplot(newdata, aes(x = month)) + 
  geom_line(aes(y = accu_money, colour = "price"),size=1.5) +
  geom_line(aes(y = acc_value, colour = "value"),size=1.5)
  #scale_y_continuous(name = "value", sec.axis = sec_axis(~./10000, name="price"))
#報酬率
ret = diff(log(newdata$as.double.mdata..0050.TW.Adjusted..))#月報酬率
mean(ret[-1])
chart.TimeSeries(cbind(ret),legend.loc="bottomleft",date.format ="%b-%Y",las = 2, ylab = "Returns",main = "Time Series Plot")

#年化報酬率
annual_rate = as.double((acc_value[length(acc_value)]/sum(monthly*mdata$`0050.TW.Adjusted`))^(1/(length(acc_value) %/% 12)) - 1)
#年化標準差： 用來衡量基金淨值一年內波動程度的指標
std = sd(ret[-1])
#Sharpe Ratio(假設無風險利率為0)
sharpe = annual_rate/std
#最大回撤
roll_max = max(newdata$as.double.mdata..0050.TW.Adjusted..)
monthlydd = (newdata$as.double.mdata..0050.TW.Adjusted..)/roll_max-1
max_dd = min(monthlydd)
#表格
rbind(acc_rate, annual_rate, sharpe, max_dd)
