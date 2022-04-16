
library(peakRAM)


c_n <- 1000
r_n <- 10000
ports_n <- 500

ret_mat <- matrix(rnorm(r_n*c_n, 0.2, 1), ncol=c_n)
wgt_mat <- matrix(runif(ports_n*c_n, 0, 1), ncol=ports_n)
wgt_mat <- wgt_mat / matrix(rep(colSums(wgt_mat),c_n), ncol=ports_n, byrow = T)



peakRAM({
  res <- NULL
  for(i in 1:ncol(wgt_mat)){
    res <- c(res, sum(ret_mat %*% wgt_mat[,i]))
  }
  rm(res)
})
#print(res)




peakRAM({
  res <- NULL
  res <- colSums(ret_mat %*% wgt_mat)
  #print(res)
  rm(res)
})

