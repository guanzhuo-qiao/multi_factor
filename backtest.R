library(quantmod)
library(TTR)
library(tseries)
back_test <- function(stck_weights,stck_returns){# time;stocks
  
  return(diag(stck_weights %*% t(stck_returns)))
}
get_te <- function(benchmark_return,portfolio_return,te_time){
  diff_return = portfolio_return-benchmark_return
  vol = sd(diff_return)
  res = vol*sqrt(12/te_time)
  return(res)
}

load("portfolio_basket.rda")
load("bt_label_data.rda")
load("bt_features.rda")
load("benchmark_label.rda")
# download benchmark
looking_back_period = 5
benchmark_label = benchmark_label[-1:-(looking_back_period+1+1)]

result_report_table = array(dim=c(dim(portfolio_basket)[1],5),dimnames = list(row_names=c("b1","b2","b3","b4"),col_names=c("crt_p","crt_b","te","mdd","sharpe")))
color = rainbow(dim(portfolio_basket)[1])
for(basket in 1:dim(portfolio_basket)[1]){
  weight = portfolio_basket[basket,,]
  rt = back_test(t(weight),t(bt_label_data))
  cp_rt = cumprod(1+rt)
  if(basket==1){
    plot(c(1:length(cp_rt)),cp_rt,type="l",xlab="time: months",ylab = "return",col=color[basket])
  }else{
    lines(c(1:length(cp_rt)),cp_rt,xlab="",ylab = "",col=color[basket])
  }
  te=get_te(benchmark_label,rt,length(rt))
  mdd = maxdrawdown(cp_rt)$maxdrawdown
  crt_b = cumprod(1+benchmark_label)[length(benchmark_label)]
  crt_p = cp_rt[length(cp_rt)]
  sharpe_rt = sharpe(rt)
  result_report_table[basket,] = c(crt_p,crt_b,te,mdd,sharpe_rt)
}
lines(c(1:length(benchmark_label)),cumprod(1+benchmark_label),xlab="",ylab = "",col="blue")
title("cumulative return")
legend("topleft",legend = c("b1","b2","b3","b4","bm"),lty = c(1),col = c(color,"blue"))





