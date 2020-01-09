library(quantmod)
library(TTR)

back_test <- function(stck_weights,stck_returns){# time;stocks
  print(cumprod(1+diag(stck_weights %*% t(stck_returns))))
  plot(c(1:dim(stck_weights)[1]),cumprod(1+diag(stck_weights %*% t(stck_returns))),type = "l")
}

load("portfolio_basket.rda")
load("bt_label_data.rda")
load("bt_features.rda")

for(basket in 1:dim(portfolio_basket)[1]){
  weight = portfolio_basket[basket,,]
  back_test(t(weight),t(bt_label_data))
}





