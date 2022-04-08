pso_pkg_obj_func <- function(wgts, v, date_interval, cov, mean_returns, env, save_stats = FALSE){
  
  intensitys <- v$algorithm$pso_pkg$settings$risk_factor_intensity
  
  wgts <- round(wgts, v$options$round_at)
  
  # maximize returns
  return <- - mean_returns %*% wgts
  
  # minimize sd/risk
  risk <- t(wgts) %*% cov %*% wgts
  
  # sum up to sum_wgts (v$constraints$sum_wgts)
  sum_wgts <- (sum(wgts) - v$constraints$sum_wgts)^2
  
  # minimize tracking error with decreasing intensity and reduced positiv errors
  te_settings <- v$algorithm$pso_pkg$settings$tracking_error
  te_intensity <- 1-(1-te_settings$reduce_historical_intensity_to) * (length(v$bm$returns[date_interval,]):1)/length(v$bm$returns[date_interval,])
  te <- te_intensity * (v$pool$returns[date_interval,] %*% wgts - v$bm$returns[date_interval,])
  te[te>0] <- te[te>0] * te_settings$reduce_positivs
  te <- sd(te)
  
  # target cardinality constrain of n (v$constraints$assets_n) assets
  asset_n <- if(!is.null(v$constraints$assets_n)){
    ((sum(wgts!=0)-v$constraints$assets_n)^2)/v$constraints$assets_n
  }else{0}
  
  # percent change on rebalancing
  change <- if(!is.null(v$fund$wgts)){
    max(sum(abs(v$fund$wgts-wgts)), 2*v$constraints$percent_change) - 2*v$constraints$percent_change
  }else{0}
  
  # allow only v$constraints$short percent in short positions
  short <- if(!is.null(v$constraints$short)){
    max(sum(abs(wgts[wgts<0])), v$constraints$short) - v$constraints$short
  }else{0}
  
  
  beta <- if(!is.null(v$constraints$beta)){
    abs(lm(v$pool$returns[date_interval,] %*% wgts ~ v$bm$returns[date_interval,])$coefficients[2] - v$constraints$beta)
  }else{0}
  
  
  obj <- intensitys$mean * return + 
    intensitys$sd * risk + 
    intensitys$sum_wgts * sum_wgts + 
    intensitys$tracking_error * te + 
    intensitys$assets_n * asset_n + 
    intensitys$percent_change * change + 
    intensitys$short * short +
    intensitys$beta * beta
  
  if(save_stats){
    env$saved_stats[[length(env$saved_stats)+1]] <- data.frame(
        "obj"=obj,
        "return"=intensitys$mean*return,
        "risk"=intensitys$sd*risk,
        "sum_wgts"=intensitys$sum_wgts*sum_wgts,
        "te"=intensitys$tracking_error*te,
        "asset_n"=intensitys$assets_n*asset_n,
        "change"=intensitys$percent_change*change,
        "short"=intensitys$short*short,
        "beta"=intensitys$beta * beta
    )
  }
  
  return(obj)
}




pso_pkg_wrapper <- function(v, save_stats = FALSE){
  
  env <- new.env()
  
  for(i in 1:length(v$options$rebalance_at)){
    start_time <- Sys.time()
    
    date <- as.Date(v$options$rebalance_at[i])
    from <- if(v$options$data_history=="1y"){date-years(1)}
    date_interval <- paste0(from,"/",date-1)
    print(paste0("i: ",i, "  intervall: ", date_interval))
    
    mean_returns <- sapply(v$pool$returns[paste0(from,"/",date-1),], mean)
    cov <- cov(v$pool$returns[paste0(from,"/",date-1),])
    
    
    opt <- psoptim(
      par = if(!is.null(v$fund$wgts)){v$fund$wgts}else{rep(0, v$pool$assets_n)},
      fn = pso_pkg_obj_func,
      v = v,
      date_interval = date_interval,
      cov = cov,
      mean_returns = mean_returns,
      env = env,
      save_stats = save_stats,
      lower = rep(0, v$pool$assets_n),
      upper = rep(1, v$pool$assets_n),
      control = list(
        maxit = v$options$iter, # max iterations
        s = 100, # swarm size
        p = 1, # percentage of information ( 1 := each particle is fully informed )
        maxit.stagnate = 20, # restart after x stagnations
        trace=0
      )
    )
    
    train_interval <- seq.Date(
      from = from,
      to = date-1,
      by = "days"
    )
    
    train_1i <- seq.Date(
      from = if(length(v$options$rebalance_at[i-1])==0){v$options$rebalance_at[i]-30}else{v$options$rebalance_at[i-1]},
      to = date-1,
      by = "days"
    )
    
    
    test_1i <- if(!is.na(v$options$rebalance_at[i+1])){
      seq.Date(
        from = date,
        to = v$options$rebalance_at[i+1]-1,
        by = "days"
      )
    }else{NULL}
    
    
    
    l <- list(
      "time" = round(as.numeric(difftime(Sys.time(), start_time, units="s"))),
      "date" = date,
      "wgts" = round(opt$par, v$options$round_at)
    )
    
    l$change <- sum(abs(v$fund$wgts-l$wgts))/2
    
    l$asset_n <- sum(l$wgts!=0)
    l$sum_wgts <- sum(l$wgts)
    
    
    res_returns <- xts(v$pool$returns %*% l$wgts, order.by=as.Date(index(v$pool$returns)))
    
    l$te_train <- sum((res_returns[train_interval,] - v$bm$returns[train_interval,])^2)
    l$te_train_1i <- sum((res_returns[train_1i,] - v$bm$returns[train_1i,])^2)
    
    if(!is.null(test_1i)){
      l$te_test_1i <- sum((res_returns[test_1i,] - v$bm$returns[test_1i,])^2)
    }
    
    if(!is.null(v$fund$wgts)){
      l$percent_change <- sum(abs(v$fund$wgts-l$wgts))
    }
    
    l$beta_train <- as.numeric(lm(v$pool$returns[train_interval,] %*% l$wgts ~ v$bm$returns[train_interval,])$coefficients[2])
    l$beta_train_1i <- as.numeric(lm(v$pool$returns[train_1i,] %*% l$wgts ~ v$bm$returns[train_1i,])$coefficients[2])
    
    if(!is.null(test_1i)){
      l$beta_test_1i <- as.numeric(lm(v$pool$returns[test_1i,] %*% l$wgts ~ v$bm$returns[test_1i,])$coefficients[2])
    }
    
    # add to global v
    v$results[[paste0("res_",v$options$rebalance_at[i])]] <<- l
  
    v$fund$wgts <<- l$wgts
    
    print(paste0("took ", l$time, " secounds"))
  }
  
  
  if(save_stats){
    saved_stats <- bind_rows(env$saved_stats)
    saved_stats <- cbind("row"=1:nrow(saved_stats), saved_stats)
    v$plots$saved_stats <<- saved_stats
    # saved_stats <- saved_stats %>%
    #   mutate(row = round(row/100)) %>%
    #   group_by(row) %>%
    #   summarise(return = sum(return),
    #             risk = sum(risk),
    #             sum_wgts = sum(sum_wgts),
    #             te = sum(te),
    #             asset_n = sum(asset_n),
    #             change = sum(change),
    #             short = sum(short),
    #             obj = sum(obj),
    #             beta = sum(beta)
    #   ) %>%
    #   ungroup() %>%
    #   mutate(obj = obj/max(obj)*100)
    # 
    # p1 <- plot_ly(data = saved_stats, x=~row, y=~return, name="return", mode="none", type = 'scatter', stackgroup="one", groupnorm="percent") %>%
    #   add_trace(y=~risk, name="risk") %>%
    #   add_trace(y=~sum_wgts, name="sum_wgts") %>%
    #   add_trace(y=~te, name="te") %>%
    #   add_trace(y=~asset_n, name="asset_n") %>%
    #   add_trace(y=~change, name="change") %>%
    #   add_trace(y=~short, name="short") %>% 
    #   add_trace(y=~beta, name="beta")
    # 
    # y_max <- mean(saved_stats$obj) #mean(last(saved_stats$obj, round(0.5*length(saved_stats$obj))))
    # p2 <- plot_ly(data = saved_stats, x=~row, y=~obj, name="obj", mode="lines", type = 'scatter') %>%
    #   layout(yaxis=list(range=c(0,y_max), ticksuffix="%"))
    # 
    # v$plots$saved_stats_plot <- subplot(p1, p2, nrows = 2)
  }
  
  
  #return(v)
}