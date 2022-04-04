# library(dplyr)
# library(xts)
# library(lubridate)
#
# source("R/calculations.R")
# source("R/plotly_charts.R")
#
# dates <- seq.Date(from = as.Date("2022-01-01"), to=as.Date("2022-01-20"), by="days")
#
#
# fund <- NULL
#
# pool_n <- 100
#
#
# bm_pool <- xts(x = sapply(0.01+0.05*(1:pool_n)/sum(1:pool_n)+rnorm(pool_n, 0, 0.0005), function(x){
#   rnorm(length(dates), mean = x, sd = x*5)#*(1+rnorm(length(dates), mean=0, sd=0.1))
# }), order.by = dates) %>%
#   setNames(paste0("asset_",1:pool_n))
#
#
# random_wgt <- abs(rnorm(pool_n, mean=1/pool_n, sd=1/pool_n))
# bm <- xts(x = bm_pool %*% (random_wgt/sum(random_wgt)), order.by = dates) %>%
#   setNames("BM")
#
# plotly_line_chart_xts(return_to_cumret(bm))
#
# bm_kurs <- return_to_cumret(bm_pool)
# #plotly_line_chart_xts(bm_kurs)
#
#
# rule <- list()
# rule$n <- round(pool_n*0.5)
#
# opt <- list()
# opt$iter <- 10000
#
#
#
# tracking_error <- function(fund_xts_return, bm_xts_return){
#   sqrt(1/length(bm_xts_return) * sum((fund_xts_return - bm_xts_return)^2))
# }
#
# excess_return <- function(fund_xts_return, bm_xts_return){
#   1/length(bm_xts_return) * sum(fund_xts_return - bm_xts_return)
# }
#
#
# fitness_f <- function(fund_xts_return, bm_xts_return, lambda){
#   lambda * tracking_error(fund_xts_return, bm_xts_return) - (1-lambda) * excess_return(fund_xts_return, bm_xts_return)
# }
#
# samples_n <- 20
#
# init_funds <- matrix(runif(pool_n*samples_n), ncol = pool_n, nrow=samples_n) %>%
#   {./rowSums(.)}
# row.names(init_funds) <- paste0("Fund_",1:samples_n)
#
#
#
# save <- NULL
# for(i in 1:opt$iter){
#
#   init_funds_return <- apply(t(init_funds), 2, function(x){
#     bm_pool %*% x
#   })
#
#   fitness_funds <- apply(init_funds_return, 2, function(x){fitness_f(fund_xts_return=x, bm_xts_return=bm, lambda=0.5)})
#
#
#   groups <- sample(c(rep(T,ceiling(samples_n/2)),rep(F,ceiling(samples_n/2))), samples_n)
#   parents <- names(c(which(fitness_funds==min(fitness_funds[groups]))[1],which(fitness_funds==min(fitness_funds[!groups]))[1]))
#   cross_rand <- sample(c(rep(T,ceiling(pool_n/2)),rep(F,ceiling(pool_n/2))), pool_n)
#   child_1 <- colSums(init_funds[parents,] * matrix(c(cross_rand, !cross_rand), ncol=pool_n, byrow = T))
#   child_1 <- child_1 * (1+rnorm(pool_n, mean = 0, sd = 0.01))
#   #child_1[!child_1 %in% child_1[child_1 >= sort(child_1, decreasing = T)[rule$n]]] <- 0
#   child_1[sample(1:length(child_1), size = sum(child_1!=0)-rule$n, prob = abs(child_1)/sum(abs(child_1)))] <- 0
#   child_1 <- child_1/sum(child_1)
#   child_2 <- colSums(init_funds[parents,] * matrix(c(!cross_rand, cross_rand), ncol=pool_n, byrow = T))
#   child_2 <- child_2 * (1+rnorm(pool_n, mean = 0, sd = 0.01))
#   #child_2[!child_2 %in% child_2[child_2 >= sort(child_2, decreasing = T)[rule$n]]] <- 0
#   child_2[sample(1:length(child_2), size = sum(child_2!=0)-rule$n, prob = abs(child_2)/sum(abs(child_2)))] <- 0
#   child_2 <- child_2/sum(child_2)
#
#   child_1_fitness <- fitness_f(fund_xts_return = bm_pool %*% child_1, bm_xts_return = bm, lambda = 0.5)
#   child_2_fitness <- fitness_f(fund_xts_return = bm_pool %*% child_2, bm_xts_return = bm, lambda = 0.5)
#   if(child_1_fitness <= max(fitness_funds)*2){
#     init_funds[names(fitness_funds[fitness_funds==max(fitness_funds)])[1],] <- child_1
#   }
#   if(child_2_fitness <= max(fitness_funds)*2){
#     init_funds[names(fitness_funds[fitness_funds==max(fitness_funds)])[1],] <- child_2
#   }
#
#   save <- rbind(save, data.frame("i"=i, "all"=mean(fitness_funds), "child_1" = child_1_fitness, "child_2" = child_2_fitness))
# }
#
#
#
# init_funds_return <- apply(t(init_funds), 2, function(x){
#   bm_pool %*% x
# })
#
# fitness_funds <- apply(init_funds_return, 2, function(x){fitness_f(fund_xts_return=x, bm_xts_return=bm, lambda=0.5)})
#
# fitness_funds
#
#
# library(plotly)
#
# suppressPlotlyMessage <- function(p) {
#   suppressMessages(plotly_build(p))
# }
#
# suppressPlotlyMessage({
#   plot_ly(data = save, x=~i) %>%
#     add_trace(y=~all, name="all", mode="lines") %>%
#     add_trace(y=~child_1, name="child_1", mode="lines") %>%
#     add_trace(y=~child_2, name="child_1", mode="lines") %>%
#     layout(yaxis = list(range=list(0,max(0.0025,max(save[,-1])*1.05))))
# })
#
#
# child_1
#
#











### nochmal
options(scipen=999)
options(stringsAsFactors = FALSE)

library(dplyr)
library(xts)
library(lubridate)
library(pso)
library(plotly)

for(src in list.files("R")){
  source(paste0("R/", src))
}

load("R-research/returns_df.rdata")
pool_returns_df <- returns_df
rownames(pool_returns_df) <- seq.Date(from = Sys.Date()-nrow(pool_returns_df)+1, to=Sys.Date(), by="days")



v <- init_optimizer(
  pool_returns_df = pool_returns_df
)

v$options$rebalance_at <- seq.Date(from = as.Date(paste0(substr(Sys.Date()-nrow(pool_returns_df)+1,1,8),"01")), to=as.Date(paste0(substr(Sys.Date(),1,8),"01")), by="months") %>% last(2)


v$constraints$assets_n <- 10


v$algorithm$pso_pkg$fun <- pso_pkg_wrapper

v <- v$algorithm$pso_pkg$fun(v=v, save_stats = TRUE)

plot_list <- linechart_backtest_returns(v)
plot_list$p_not_split
plot_list$p_split
