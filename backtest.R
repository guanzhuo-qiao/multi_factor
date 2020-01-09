library(quantmod)
library(TTR)

back_test <- function(stck_weights,stck_returns){
  prod(diag(stck_weights %*% t(stck_returns)))
}
