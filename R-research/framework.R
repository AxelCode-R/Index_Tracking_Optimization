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

init_optimizer <- function(pool_returns_df, bm_wgts){

  v <- list(
    "pool" = list(
      "returns" = NULL,
      #"mean_returns" = NULL,
      "assets_n" = NULL,
      "days_n" = NULL#,
      #"cov" = NULL
    ),
    "fund" = list(
      "wgts" = NULL,
      "returns" = NULL
    ),
    "algorithm" = list(
      "pso_pkg" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("mean"=1, "sd"=10, "sum_wgts"=10, "tracking_error"=10, "assets_n"=10, "percent_change"=10, "short"=10),
          "tracking_error" = list("reduce_historical_intensity_to"=0.3, "reduce_positivs"=0.5)
        )
      )
    ),
    "bm" = list(
      "wgts" = NULL,
      "returns" = NULL
    ),
    "options" = list(
      "iter" = 5000,
      "rebalance_at" = NULL,
      "round_at" = 6
    ),
    "constraints" = list(
      "sum_wgts" = 1,
      "assets_n" = NULL,
      "percent_change" = 0.2,
      "short" = 0
    ),
    "results" = list(
    )
  )

  v$pool$returns <- xts(pool_returns_df, order.by = as.Date(rownames(pool_returns_df)))
  #v$pool$mean_returns <- sapply(v$pool$returns, mean)
  v$pool$assets_n <- ncol(v$pool$returns)
  v$pool$days_n <- nrow(v$pool$returns)
  #v$pool$cov <- cov(v$pool$returns)

  v$bm$wgts <- rep(1/ncol(v$pool$returns),ncol(v$pool$returns))

  v$bm$returns <- xts(v$pool$returns %*% rep(1/v$pool$assets_n,v$pool$assets_n), order.by=index(v$pool$returns))

  return(v)
}

v <- init_optimizer(
  pool_returns_df = pool_returns_df
)

v$options$rebalance_at <- seq.Date(from = as.Date(paste0(substr(Sys.Date()-nrow(pool_returns_df)+1,1,8),"01")), to=as.Date(paste0(substr(Sys.Date(),1,8),"01")), by="months") %>% last(6)
# plotly_line_chart_xts(return_to_cumret(bm$returns))

v$constraints$assets_n <- 10



obj_func <- function(wgts, v, data_date, cov, mean_returns, save_stats = FALSE){

  intensitys <- v$algorithm$pso_pkg$settings$risk_factor_intensity


  # # maximize returns
  # obj <- - intensitys$mean * (mean_returns %*% wgts)
  #
  # # minimize sd/risk
  # obj <- obj + intensitys$sd * (t(wgts) %*% cov %*% wgts)
  #
  # # sum up to sum_wgts (v$constraints$sum_wgts)
  # obj <- obj + intensitys$sum_wgts * (sum(wgts) - v$constraints$sum_wgts)^2
  #
  # # minimize tracking error with decreasing intensity and reduced positiv errors
  # te_settings <- v$algorithm$pso_pkg$settings$tracking_error
  # te_intensity <- 1-(1-te_settings$reduce_historical_intensity_to) * (length(v$bm$returns[paste0("/",data_date-1),]):1)/length(v$bm$returns[paste0("/",data_date-1),])
  # te <- te_intensity * (v$pool$returns[paste0("/",data_date-1),] %*% wgts - v$bm$returns[paste0("/",data_date-1),])
  # te[te>0] <- te[te>0] * te_settings$reduce_positivs
  # obj <- obj + intensitys$tracking_error * sum((te)^2)
  #
  # # target cardinality constrain of n (v$constraints$assets_n) assets
  # if(!is.null(v$constraints$assets_n)){
  #   obj <- obj + intensitys$assets_n * ((sum(round(wgts, v$options$round_at)!=0)-v$constraints$assets_n)/v$pool$assets_n)^2
  # }
  #
  # # percent change on rebalancing
  # if(!is.null(v$fund$wgts)){
  #   obj <- obj + intensitys$percent_change * (max(sum(abs(v$fund$wgts-wgts)), v$constraints$percent_change) - v$constraints$percent_change)
  # }
  #

  # maximize returns
  return <- - mean_returns %*% wgts

  # minimize sd/risk
  risk <- t(wgts) %*% cov %*% wgts

  # sum up to sum_wgts (v$constraints$sum_wgts)
  sum_wgts <- (sum(wgts) - v$constraints$sum_wgts)^2

  # minimize tracking error with decreasing intensity and reduced positiv errors
  te_settings <- v$algorithm$pso_pkg$settings$tracking_error
  te_intensity <- 1-(1-te_settings$reduce_historical_intensity_to) * (length(v$bm$returns[paste0("/",data_date-1),]):1)/length(v$bm$returns[paste0("/",data_date-1),])
  te <- te_intensity * (v$pool$returns[paste0("/",data_date-1),] %*% wgts - v$bm$returns[paste0("/",data_date-1),])
  te[te>0] <- te[te>0] * te_settings$reduce_positivs
  te <- sum((te)^2)

  # target cardinality constrain of n (v$constraints$assets_n) assets
  asset_n <- if(!is.null(v$constraints$assets_n)){
    ((sum(round(wgts, v$options$round_at)!=0)-v$constraints$assets_n)/v$pool$assets_n)^2
  }else{0}

  # percent change on rebalancing
  change <- if(!is.null(v$fund$wgts)){
    max(sum(abs(v$fund$wgts-wgts)), v$constraints$percent_change) - v$constraints$percent_change
  }else{0}

  # allow only v$constraints$short percent in short positions
  short <- max(sum(abs(wgts[wgts<0])), v$constraints$short) - v$constraints$short

  obj <- intensitys$mean * return + intensitys$sd * risk + intensitys$sum_wgts * sum_wgts + intensitys$tracking_error * te + intensitys$assets_n * asset_n + intensitys$percent_change * change + intensitys$short * short

  if(save_stats){
    if(!exists("obj_stats")){obj_stats <<- NULL}
    obj_stats <<- rbind(
      obj_stats,
      data.frame(
        "return"=return,
        "risk"=risk,
        "sum_wgts"=sum_wgts,
        "te"=te,
        "asset_n"=asset_n,
        "change"=change,
        "short"=short,
        "obj"=obj,
        "return2"=intensitys$mean*return,
        "risk2"=intensitys$sd*risk,
        "sum_wgts2"=intensitys$sum_wgts*sum_wgts,
        "te2"=intensitys$tracking_error*te,
        "asset_n2"=intensitys$assets_n*asset_n,
        "change2"=intensitys$percent_change*change,
        "short2"=intensitys$short*short
      )
    )
  }

  return(obj)
}


v$algorithm$pso_pkg$fun <- function(v, save_stats = FALSE){

  for(i in 1:length(v$options$rebalance_at)){
    print(i)

    date <- as.Date(v$options$rebalance_at[i])
    mean_returns <- sapply(v$pool$returns[paste0("/",date-1),], mean)
    cov <- cov(v$pool$returns[paste0("/",date-1),])

    opt <- psoptim(
      par = if(!is.null(v$fund$wgts)){v$fund$wgts}else{rep(0, v$pool$assets_n)},
      fn = obj_func,
      v = v,
      data_date = date-1,
      cov = cov,
      mean_returns = mean_returns,
      save_stats = save_stats,
      lower = rep(0, v$pool$assets_n),
      upper = rep(1, v$pool$assets_n),
      control = list(
        maxit = 100,
        s = 100,
        maxit.stagnate = 500,
        trace=1
      )
    )


    train_interval <- seq.Date(
      from = if(length(v$options$rebalance_at[i-1])==0){v$options$rebalance_at[i]-30}else{v$options$rebalance_at[i-1]},
      to = date-1,
      by = "days"
    )


    test_interval <- if(!is.na(v$options$rebalance_at[i+1])){
      seq.Date(
        from = date,
        to = v$options$rebalance_at[i+1]-1,
        by = "days"
      )
    }else{NULL}



    l <- list(
      "date" = date,
      "wgts" = round(opt$par, v$options$round_at)
    )

    res_returns <- xts(v$pool$returns %*% l$wgts, order.by=as.Date(index(v$pool$returns)))

    l$tracking_error_train <- sum((res_returns[train_interval,] - v$bm$returns[train_interval,])^2)

    if(!is.null(test_interval)){
      l$tracking_error_test <- sum((res_returns[test_interval,] - v$bm$returns[test_interval,])^2)
    }

    if(!is.null(v$fund$wgts)){
      l$percent_change <- sum(abs(v$fund$wgts-l$wgts))
    }

    v$results[[paste0("res_",v$options$rebalance_at[i])]] <- l


    v$fund$wgts <- l$wgts
  }


  return(v)
}


v <- v$algorithm$pso_pkg$fun(v=v, save_stats = TRUE)
obj_stats2 <- cbind("row"=1:nrow(obj_stats), obj_stats)
#View(obj_stats)
#plotly_line_chart_xts(return_to_cumret(cbind.xts("Fund"=v$fund$returns, "BM"=v$bm$returns)))
# plot_ly() %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2, name="return2", mode="lines", type = 'scatter') %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2+obj_stats$risk2, name="risk2", mode="lines", type = 'scatter') %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2+obj_stats$risk2+obj_stats$sum_wgts2, name="sum_wgts2", mode="lines", type = 'scatter') %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2+obj_stats$risk2+obj_stats$sum_wgts2+obj_stats$te2, name="te2", mode="lines", type = 'scatter') %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2+obj_stats$risk2+obj_stats$sum_wgts2+obj_stats$te2+obj_stats$asset_n2, name="asset_n2", mode="lines", type = 'scatter') %>%
#   add_trace(x=obj_stats$row, y=obj_stats$return2+obj_stats$risk2+obj_stats$sum_wgts2+obj_stats$te2+obj_stats$asset_n2+obj_stats$change2, name="change2", mode="lines", type = 'scatter') %>%
#   #add_trace(x=obj_stats$row, y=obj_stats$obj, name="obj", mode="lines", type = 'scatter') %>%
#   layout(
#     yaxis = list(range=c(0,10))
#   )

obj_stats2 <- obj_stats2 %>%
  mutate(row = round(row/100)) %>%
  group_by(row) %>%
  summarise(return2 = sum(return2),
            risk2 = sum(risk2),
            sum_wgts2 = sum(sum_wgts2),
            te2 = sum(te2),
            asset_n2 = sum(asset_n2),
            change2 = sum(change2),
            short2 = sum(short2),
            obj = sum(obj)
            ) %>%
  ungroup() %>%
  mutate(obj = obj/max(obj)*100)

#plotly_line_chart_xts(return_to_cumret(cbind.xts("Fund"=v$fund$returns, "BM"=v$bm$returns)))
p1 <- plot_ly(data = obj_stats2, x=~row, y=~return2, name="return2", mode="none", type = 'scatter', stackgroup="one", groupnorm="percent") %>%
  add_trace(y=~risk2, name="risk2") %>%
  add_trace(y=~sum_wgts2, name="sum_wgts2") %>%
  add_trace(y=~te2, name="te2") %>%
  add_trace(y=~asset_n2, name="asset_n2") %>%
  add_trace(y=~change2, name="change2") %>%
  add_trace(y=~short2, name="short2")# %>%
  #add_trace(y=~obj, name="obj", mode="lines+scatter", type = 'scatter')

p2 <- plot_ly(data = obj_stats2, x=~row, y=~obj, name="obj", mode="lines", type = 'scatter') %>%
  layout(yaxis=list(range=c(0,0.1), ticksuffix="%"))

subplot(p1, p2, nrows = 2)


linechart_backtest_returns <- function(v){

  all_returns_not_split <- NULL
  all_returns_split <- NULL
  annotation_data <- NULL
  for(i in 1:length(v$results)){
    res <- v$results[[i]]

    from <- res$date
    to <- if(i != length(v$results)){v$results[[i+1]]$date-1}else{max(index(v$pool$returns))}

    returns <- cbind.xts(
      "Fund" = xts(v$pool$returns[paste0(from,"/",to),] %*% res$wgts, order.by=as.Date(index(v$bm$returns[paste0(from,"/",to),]))),
      "BM" = v$bm$returns[paste0(from,"/",to),]
    )


    df_returns_not_split <- data.frame("Date"=index(returns), as.data.frame(returns))
    rownames(df_returns_not_split) <- NULL

    all_returns_not_split <- bind_rows(
      all_returns_not_split,
      df_returns_not_split
    )


    returns <- return_to_cumret(returns)

    df_returns_split <- data.frame("Date"=c(index(returns),last(index(returns))), rbind(as.data.frame(returns),c(NA,NA)))
    rownames(df_returns_split) <- NULL

    all_returns_split <- bind_rows(
      all_returns_split,
      df_returns_split
    )

    annotation_data <- bind_rows(
      annotation_data,
      data.frame(
        "Date"=res$date,
        "TE_train"=round(res$tracking_error_train,6),
        "TE_test"=if(!is.null(res$tracking_error_test)){round(res$tracking_error_test,6)}else{NA},
        "change"=if(is.null(res$percent_change)){"100 %"}else{paste0(round(res$percent_change,4)*100," %")},
        "Alpha"=paste0(round(last(returns$Fund-returns$BM),2), " %"))
    )

  }

  all_returns_not_split <- return_to_cumret(xts(all_returns_not_split[,-1], order.by=as.Date(all_returns_not_split$Date)))
  p_not_split <- plotly_line_chart_xts(all_returns_not_split)



  p_split <- plot_ly() %>%
    add_trace(x=all_returns_split$Date, y=all_returns_split$Fund, name="Fund", mode="lines", type = 'scatter') %>%
    add_trace(x=all_returns_split$Date, y=all_returns_split$BM, name="BM", mode="lines", type = 'scatter') %>%
    add_annotations(
      x = annotation_data$Date,
      y = max(all_returns_split[,2:3], na.rm = T)*1.1,
      text = paste0("TE_train: ", annotation_data$TE_train, "\nTE_test: ", annotation_data$TE_test, "\nAlpha: ", annotation_data$Alpha, "\nchange: ", annotation_data$change),
      xref = "x",
      yref = "y",
      showarrow=F) %>%
    layout(
      xaxis = list(
        type = 'date',
        tickformat = "%Y-%m-%d",
        tickvals=c(annotation_data$Date, max(all_returns_split$Date))
        )
    )


  return(list("p_not_split"=p_not_split, "p_split"=p_split))

}


plot_list <- linechart_backtest_returns(v)
plot_list$p_not_split
plot_list$p_split
