library(quantmod)
library(TTR)
library(Rglpk)

load("factor_table.rda")
load("label_table.rda")
load("stock_list.rda")

data_cleaner <- function(data_set){
  for(i in 2:dim(data_set)[2]){# exclude first one due to NaN
    cross_sec_data = data_set[,i,]
    cross_sec_data = t(na.omit(t(cross_sec_data)))
    # process extreme values
    med = apply(cross_sec_data,2,median)
    med_matrix = sapply(med,rep,dim(cross_sec_data)[1])
    mad = apply(abs(cross_sec_data-med_matrix),2,median)
    
    rank_table = apply(cross_sec_data, 2, rank)
    
    ceiling = med+3*1.483*mad
    floor = med-3*1.483*mad
    
    for(j in 1:dim(cross_sec_data)[2]){
      tmp = cross_sec_data[,j]
      top_index = which(tmp>ceiling[j])
      top_rank = rank_table[top_index,j]
      if(mad[j]!=0){
      cross_sec_data[top_index,j] = ceiling[j]+0.5*1.483*mad[j]/(length(top_rank)+1)*(top_rank-(length(tmp)-length(top_rank)))
      }
      bottom_index = which(tmp<floor[j])
      bottom_rank = rank_table[bottom_index,j]
      if(mad[j]!=0){
      cross_sec_data[bottom_index,j] = floor[j]-0.5*1.483*mad[j]/(length(bottom_rank)+1)*(length(bottom_rank)+1-bottom_rank)
      }
    }
    # normalize
    cross_sec_data <- scale(cross_sec_data)
    data_set[,i,] = cross_sec_data
  }
  data_set[,-1,]
}

features = data_cleaner(features)

data_orthogonal <- function(data_set){
  for(i in 1:dim(data_set)[2]){
    cross_sec_data = data_set[,i,]
    m = t(cross_sec_data)%*%cross_sec_data
    eigen_of_m = eigen(m,symmetric=T)
    d = diag(1/sqrt(eigen_of_m$values))
    u = eigen_of_m$vectors
    s = u%*%d%*%t(u)
    data_set[,i,] = cross_sec_data%*%s
    print(i)
  }
  data_set
}

features = data_orthogonal(features)

# weighted sum according to ICIR

get_sum_factor <- function(label_data,feature_data,looking_back){
  # looking_back: control the rolling back period of ICIR
  # exclude the first column due to the lack of features' data
  # exclude the second on due to the need of lag 1 period data
  lag_label_data = label_data[,c(-1,-2),] # 2d table
  lag_feature_data = feature_data[,1:dim(feature_data)[2]-1,]
  cor_table = array(dim=c(dim(lag_feature_data)[2],dim(lag_feature_data)[3])) # row: time; column: factors
  for(layer in 1:dim(lag_feature_data)[2]){
    cc = apply(X=lag_feature_data[,layer,],MARGIN = 2,FUN=cor,y=lag_label_data[,layer],method="spearman")
    cor_table[layer,] = cc
    #print(layer)
  }
  ir_table = rollapply(data=cor_table,width=looking_back,FUN=mean)/rollapply(data=cor_table,width=looking_back,FUN=sd)
  #weighted sum
  new_features_data = lag_feature_data[,-1:(-looking_back+1),]
  result_reatures_data = array(dim = c(dim(new_features_data)[1],dim(new_features_data)[2]))
  for(layer in 1:dim(new_features_data)[2]){
    weight = ir_table[layer,]
    cross_sec_data = new_features_data[,layer,]
    sum_factor = apply(cross_sec_data, MARGIN=1, FUN="%*%", weight)
    result_reatures_data[,layer] = sum_factor #row: stock; column:time
  }
  result_reatures_data
}
looking_back_period = 5
new_features = get_sum_factor(label_table,features,looking_back_period)

# select the stocks subject to new factors

bt_label_data = label_table[,-1:-(looking_back_period+1+1),] # the feature use looking back period and 1 future return to get.
bt_features = new_features[,-dim(new_features)[2]] #the last data can't be tested due to outrage of the time period

get_stock_weight <- function(factor_return_table,type_){
  # according to tf securities
  if(type_=="linear"){
    # optimizer here
    # max r'w
    #  s.t. 0<= w <= 0.4
    #       sum(w)==1
    obj <- factor_return_table
    mat <- matrix(rep(1,length(factor_return_table)),nrow=1)
    dir <- c("==")
    rhs <- c(1)
    max <- TRUE
    bounds <- list(lower = list(ind = 1:length(factor_return_table), val = rep(0,length(factor_return_table))),
                   upper = list(ind = 1:length(factor_return_table), val = rep(0.4,length(factor_return_table))))
    res = Rglpk_solve_LP(obj, mat, dir, rhs, bounds, max = max)
    return(res$solution)
  }
  if(type_=="equal"){
    return(rep(1/length(factor_return_table),length(factor_return_table)))
  }
}


get_quantile_portfolios <- function(sortable_features,quantile_num){
  quantile_portfolios = array(dim=c(quantile_num,dim(sortable_features))) # quantile_layer;stock;time
  divide = dim(quantile_portfolios)[2]%/%quantile_num
  rank_table = apply(sortable_features,2,rank)
  for(time in 1:dim(sortable_features)[2]){
    count = 1
    for(basket in 1:quantile_num){
      tmp = rep(0,dim(quantile_portfolios)[2])
      if(basket==quantile_num){
        index_ = which(rank_table[,time]>=count)
      }else{
        index_ = which(rank_table[,time]>=count & rank_table[,time]<count+divide)
        count = count+divide
      }
      tmp[index_] = get_stock_weight(sortable_features[index_,time],type_="equal")
      quantile_portfolios[basket,,time] = tmp
    }
  }
  quantile_portfolios
}

portfolio_basket = get_quantile_portfolios(bt_features,quantile_num=4)








save(portfolio_basket,file="portfolio_basket.rda")
save(bt_label_data,file="bt_label_data.rda")
save(bt_features,file="bt_features.rda")








