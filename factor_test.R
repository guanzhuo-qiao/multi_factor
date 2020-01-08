library(quantmod)
library(TTR)

load("factor_table.rda")
load("label_table.rda")
load("stock_list.rda")

data_cleaner <- function(data_set){
  for(i in 2:dim(data_set)[2]){
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
      cross_sec_data[top_index,j] = ceiling[j]+0.5*1.483*mad[j]/(length(top_rank)+1)*(top_rank-(length(tmp)-length(top_rank)))
      
      bottom_index = which(tmp<floor[j])
      bottom_rank = rank_table[bottom_index,j]
      cross_sec_data[bottom_index,j] = floor[j]-0.5*1.483*mad[j]/(length(bottom_rank)+1)*(length(bottom_rank)+1-bottom_rank)
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






