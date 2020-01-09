library(quantmod)
library(TTR)
#getSymbols("AAPL",from = "2017-01-01",to = "2020-01-01",src = "yahoo")
#holistic_data = AAPL
#setwd("~/Desktop")


get_factor <- function(one_month_data){
  # here we get some inner monthly factor
  month_high = max(one_month_data[,2])
  month_low = min(one_month_data[,3])
  month_close = last(one_month_data[dim(one_month_data)[1]])
  month_volume = sum(one_month_data[,5])
  # W&R
  month_wr = (month_high-month_close)/(month_high-month_low)
  # Close Location Value
  month_clv = ((month_close-month_low)-(month_high-month_close))/(month_high-month_low)
  # Accumulation/Distribution Line
  month_adl = month_clv*month_volume
  out = c(month_wr,month_clv)
  out
}

get_devfactor <- function(whole_data, N_ema_fast, N_ema_slow)
{
  pema_fast <- coredata(EMA(whole_data[,6],n = N_ema_fast))
  pema_slow <- coredata(EMA(whole_data[,6],n = N_ema_slow))
  vema_fast <- coredata(EMA(whole_data[,5],n = N_ema_fast))
  vema_slow <- coredata(EMA(whole_data[,5],n = N_ema_slow))
  ppo = 1-pema_slow/pema_fast
  pvo = 1-vema_slow/vema_fast
  wr = TTR::WPR(whole_data[,2:4])
  clv = TTR::CLV(whole_data[,2:4])
  ad = TTR::chaikinAD(whole_data[,2:4],whole_data[,5])
  so = TTR::stoch(whole_data[,2:4])[,"slowD"]
  macd = TTR::MACD(whole_data[,6])[,"macd"]
  bb = TTR::BBands(whole_data[,2:4])[,"pctB"]
  cmf = TTR::CMF(whole_data[,2:4],whole_data[,5])
  rsi = TTR::RSI(whole_data[,6])
  
  results = xts(matrix(c(ppo,pvo,wr,clv,ad,so,macd,bb,cmf,rsi),ncol=10),order.by = index(whole_data))
  results
}

#factor_data = split.xts(get_devfactor(holistic_data,12,26),f="months")

get_month_devfactor <- function(table){
  month_last_obv = last(coredata(last(table)))
  month_last_obv
}

#monthly_factor = matrix(unlist(lapply(factor_data,get_month_devfactor)),ncol=10,byrow = T)

# input is xts object (one month data)
get_return <- function(one_month_data){
  month_close = coredata(last(one_month_data)[,6])
  month_open = coredata(first(one_month_data)[,6])
  month_return = month_close/month_open-1
  month_return
}
# apply function on the list of monthly grouped data
#monthly_return = c(unlist(lapply(monthly_data,get_return)))

stock_list = c("KO","CAT","JPM","NKE","MMM","TRV","JNJ","PFE","PG","IBM","WMT","UTX","VZ","V",
"MSFT","INTC","BA","CVX","CSCO","MRK","AXP","HD","DIS","UNH","XOM","AAPL","WBA","GS","MCD")

features <- array(dim=c(length(stock_list),36,10))# stocks number; months number; features number
label_table <- array(dim=c(length(stock_list),36,1))
count = 0
for(stock_symbols in stock_list){
  count = count+1
  raw_data <- getSymbols(stock_symbols,from="2017-01-01",to="2020-01-01",src="yahoo",auto.assign=FALSE)
  monthly_data <- split.xts(raw_data,f="months")
  factor_data = split.xts(get_devfactor(raw_data,12,26),f="months")
  monthly_factor = matrix(unlist(lapply(factor_data,get_month_devfactor)),ncol=10,byrow = T)
  features[count,,] = monthly_factor
  label_table[count,,] = c(unlist(lapply(monthly_data,get_return)))
  print(stock_symbols)
}
save(features,file="factor_table.rda")
save(label_table,file="label_table.rda")
save(stock_list,file="stock_list.rda")



