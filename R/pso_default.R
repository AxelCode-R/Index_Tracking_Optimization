pso_default_obj_func <- function(wgts, v, date_interval, cov, mean_returns, prices, env, save_stats = FALSE){
  
  intensitys <- v$algorithm$pso_pkg$settings$risk_factor_intensity
  
  #wgts <- round(wgts, v$options$round_at)
  # wgts <- as.vector(floor(wgts*v$options$NAV/coredata(prices)) * coredata(prices) / v$options$NAV)
  # wgts[wgts*v$options$NAV < v$options$min_transaction_size] <- 0
  
  return <- - mean_returns %*% wgts
  
  risk <- sqrt(t(wgts) %*% cov %*% wgts)
  
  #sharp-ratio
  sharp_ratio <- if(risk > 0){
    return/risk
  }else{
    0
  }
  
  
  
  # minimize tracking error with decreasing intensity and reduced positiv errors
  te_settings <- v$algorithm$pso_pkg$settings$tracking_error
  te_intensity <- 1-(1-te_settings$reduce_historical_intensity_to) * (length(v$bm$returns[date_interval,]):1)/length(v$bm$returns[date_interval,])
  te <- te_intensity * (v$pool$returns[date_interval,] %*% wgts - v$bm$returns[date_interval,])
  te[te>0] <- te[te>0] * te_settings$reduce_positivs
  te <- sd(te)
  
  
  # percent change on rebalancing
  change <- if(!is.null(v$fund$wgts)){
    max(sum(abs(v$fund$wgts-wgts)), 2*v$constraints$change) - 2*v$constraints$change
  }else{0}
  
  
  beta <- if(!is.null(v$constraints$beta)){
    abs(lm(v$pool$returns[date_interval,] %*% wgts ~ v$bm$returns[date_interval,], na.action=na.omit)$coefficients[2] - v$constraints$beta)
  }else{0}
  
  
  obj <- 
    intensitys$return * return +
    intensitys$risk * risk +
    intensitys$sharp_ratio * sharp_ratio + 
    intensitys$tracking_error * te + 
    intensitys$change * change + 
    intensitys$beta * beta
  
  # if(save_stats){
  #   env$saved_stats[[length(env$saved_stats)+1]] <- data.frame(
  #     "obj"=obj,
  #     "sharp_ratio"=intensitys$sharp_ratio*sharp_ratio,
  #     "sum_wgts"=intensitys$sum_wgts*sum_wgts,
  #     "te"=intensitys$tracking_error*te,
  #     "asset_n"=intensitys$assets_n*asset_n,
  #     "change"=intensitys$change*change,
  #     "short"=intensitys$short*short,
  #     "beta"=intensitys$beta * beta
  #   )
  # }
  
  return(obj)
}



pso_default <- function(
  par, 
  fn, 
  control = list(
    maxit = 100, # max iterations
    s = 100, # number of particles
    maxV = 0.5, # max velocity
    inertia = 0.2, # inertia weight 
    c1 = 0.5,
    c2 = 0.5,
    cardinal_n = 50, # par unequal 0
    max_wgt = 0.04,
    sum_wgt = 1
  ), 
  v,
  date_interval,
  cov,
  mean_returns,
  prices,
  env,
  save_stats){
  # Book Applying PSO S.173 (10.1)
  n <- length(par)
  fn1 <- function(par){
    fn(
      par, 
      v = v,
      date_interval = date_interval,
      cov = cov,
      mean_returns = mean_returns,
      prices = prices,
      env = env,
      save_stats = save_stats
    )
  }
  max_wgts <- rep(control$max_wgt, n)

  #browser()
  
  # init velocity
  p.vel <- matrix(runif(n*control$s,-1,1), ncol=control$s) * max_wgts * control$maxV
  
  # init particle position
  p.pos <- matrix(runif(n*control$s,0,1), ncol=control$s) * max_wgts
  
  # cardinality
  for(k in 1:control$s){
    p.pos[order(p.pos[, k], decreasing = T)[-(1:control$cardinal_n)], k] <- 0
    p.pos[, k] <- control$sum_wgt * p.pos[, k] / max(sum(p.pos[, k]), 0.00001)
    p.pos[, k] <- as.vector(floor(p.pos[, k]*v$options$NAV/coredata(prices)) * coredata(prices) / v$options$NAV)
  }
  
  # fittness
  p.fit <- apply(p.pos, 2, fn1)
  
  # init local best
  p.lb <- p.pos
  
  # init fittness of all local best
  p.lb.fit <- p.fit
  
  # init global best
  p.gb <- p.lb[, which.min(p.lb.fit)]
  
  # init fittness of global best
  p.gp.fit <- min(p.lb.fit)
  
  i <- 0
  while(i<=control$maxit){
    i <- i + 1
    
    # new velocity
    p.vel <- control$inertia * p.vel + 
      control$c1 * matrix(runif(n*control$s,0,1), ncol=control$s) * ( p.lb - p.pos ) +
      control$c2 * matrix(runif(n*control$s,0,1), ncol=control$s) * ( p.gb - p.pos )
    
    # new position
    p.pos <- p.pos + p.vel

    # cardinality
    for(k in 1:control$s){
      p.pos[order(p.pos[, k], decreasing = T)[-(1:control$cardinal_n)], k] <- 0
      p.pos[, k] <- control$sum_wgt * p.pos[, k] / max(sum(p.pos[, k]), 0.00001)
      p.pos[, k] <- as.vector(floor(p.pos[, k]*v$options$NAV/coredata(prices)) * coredata(prices) / v$options$NAV)
    }
    
    # fittness
    p.fit <- apply(p.pos, 2, fn1)
    
    # init local best
    p.lb[, p.lb.fit > p.fit] <- p.pos[, p.fit < p.lb.fit]
    
    # init fittness of all local best
    p.lb.fit[p.lb.fit > p.fit] <- p.fit[p.fit < p.lb.fit]
    
    # init global best
    p.gb <- p.lb[, which.min(p.lb.fit)]
    
    # init fittness of global best
    p.gp.fit <- min(p.lb.fit)
    
    message(paste0("fittness: ", p.gp.fit))
  }
  
  return(p.gb)
}


pso_default_wrapper <- function(v, save_stats = FALSE){
  env <- new.env()
  
  for(i in 1:length(v$options$rebalance_at)){
    start_time <- Sys.time()
    
    date <- as.Date(v$options$rebalance_at[i])
    from <- date-days(v$options$data_history)
    date_interval <- paste0(from,"/",date-1)
    print(paste0("i: ",i, "  intervall: ", date_interval))
    
    mean_returns <- sapply(v$pool$returns[date_interval,], mean)
    cov <- cov(v$pool$returns[date_interval,])
    prices <- v$pool$prices[last(which(index(v$pool$prices) <= (date-1))),]
    
    opt <- pso_default(
      par = if(!is.null(v$fund$wgts)){v$fund$wgts}else{rep(0, v$pool$assets_n)},
      fn = pso_default_obj_func,
      v = v,
      date_interval = date_interval,
      cov = cov,
      mean_returns = mean_returns,
      prices = prices,
      env = env,
      save_stats = save_stats
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
      "wgts" = opt
    )
    
    l$wgts <- as.vector(floor(l$wgts*v$options$NAV/coredata(prices)) * coredata(prices) / v$options$NAV)
    l$wgts[l$wgts*v$options$NAV < v$options$min_transaction_size] <- 0
    names(l$wgts) <- names(v$pool$returns)
    
    l$anzahl <- l$wgts*v$options$NAV/coredata(prices)
    
    l$risk <- t(l$wgts) %*% cov %*% l$wgts
    
    l$mean_return <- mean_returns %*% l$wgts
    
    l$sharp_ratio <- l$mean_return/l$risk
    
    l$change <- if(!is.null(v$fund$wgts)){
      sum(abs(v$fund$wgts-l$wgts))/2
    }else{
      sum(abs(l$wgts))
    }
    
    l$asset_n <- sum(l$wgts!=0)
    l$sum_wgts <- sum(l$wgts)
    
    
    res_returns <- xts(v$pool$returns %*% l$wgts, order.by=as.Date(index(v$pool$returns)))
    
    l$te_train <- sum((res_returns[train_interval,] - v$bm$returns[train_interval,])^2)
    l$te_train_1i <- sum((res_returns[train_1i,] - v$bm$returns[train_1i,])^2)
    
    if(!is.null(test_1i)){
      l$te_test_1i <- sum((res_returns[test_1i,] - v$bm$returns[test_1i,])^2)
    }
    
    
    l$beta_train <- as.numeric(lm(v$pool$returns[train_interval,] %*% l$wgts ~ v$bm$returns[train_interval,])$coefficients[2])
    l$beta_train_1i <- as.numeric(lm(v$pool$returns[train_1i,] %*% l$wgts ~ v$bm$returns[train_1i,])$coefficients[2])
    
    if(!is.null(test_1i)){
      l$beta_test_1i <- as.numeric(lm(v$pool$returns[test_1i,] %*% l$wgts ~ v$bm$returns[test_1i,])$coefficients[2])
    }
    
    # add to global v
    v$results[[paste0("res_",v$options$rebalance_at[i])]] <- l
    
    v$fund$wgts <- l$wgts
    
    v <<- v
    
    print(paste0("took ", l$time, " secounds"))
  }
  
  
  if(save_stats){
    saved_stats <- bind_rows(env$saved_stats)
    saved_stats <- cbind("row"=1:nrow(saved_stats), saved_stats)
    v$plots$saved_stats <<- saved_stats
  }
  
  
  
}
