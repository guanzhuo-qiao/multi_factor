library(quantmod)
library(TTR)

back_test <- function(stck_weights,stck_returns){# time;stocks
  print(cumprod(1+diag(stck_weights %*% t(stck_returns))))
  plot(c(1:dim(stck_weights)[1]),cumprod(1+diag(stck_weights %*% t(stck_returns))),type = "l")
}

load("portfolio_basket.rda")
load("bt_label_data.rda")
load("bt_features.rda")

# download benchmark
dji = getSymbols("^DJI",from = "2017-01-01",to = "2020-01-01",src = "yahoo",auto.assign=FALSE)

get_return <- function(one_month_data){
  month_close = coredata(last(one_month_data)[,6])
  month_open = coredata(first(one_month_data)[,6])
  month_return = month_close/month_open-1
  month_return
}
monthly_data = split.xts(dji,f="months")
benchmark_label = c(unlist(lapply(monthly_data,get_return)))
benchmark_label = benchmark_label[-1:-(looking_back_period+1+1)]


for(basket in 1:dim(portfolio_basket)[1]){
  weight = portfolio_basket[basket,,]
  back_test(t(weight),t(bt_label_data))
  par(new=T)
}





