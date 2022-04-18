# SPARSE VELOCITY PSO

svpso_obj_func <- function(wgts, v, date_interval, cov, mean_returns, prices, env, save_stats = FALSE){
  
  
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
    max(sum(abs(v$fund$wgts-wgts)), 2*v$constraints$change) - 2*v$constraints$change
  }else{0}
  
  # allow only v$constraints$short percent in short positions
  short <- if(!is.null(v$constraints$short)){
    max(sum(abs(wgts[wgts<0])), v$constraints$short) - v$constraints$short
  }else{0}
  
  
  beta <- if(!is.null(v$constraints$beta)){
    abs(lm(v$pool$returns[date_interval,] %*% wgts ~ v$bm$returns[date_interval,], na.action=na.omit)$coefficients[2] - v$constraints$beta)
  }else{0}
  
  
  obj <- 
    intensitys$return * return +
    intensitys$risk * risk +
    intensitys$sharp_ratio * sharp_ratio + 
    intensitys$sum_wgts * sum_wgts + 
    intensitys$tracking_error * te + 
    intensitys$assets_n * asset_n + 
    intensitys$change * change + 
    intensitys$short * short +
    intensitys$beta * beta
  
  if(save_stats){
    env$saved_stats[[length(env$saved_stats)+1]] <- data.frame(
      "obj"=obj,
      "sharp_ratio"=intensitys$sharp_ratio*sharp_ratio,
      "sum_wgts"=intensitys$sum_wgts*sum_wgts,
      "te"=intensitys$tracking_error*te,
      "asset_n"=intensitys$assets_n*asset_n,
      "change"=intensitys$change*change,
      "short"=intensitys$short*short,
      "beta"=intensitys$beta * beta
    )
  }
  
  return(obj)
}

svpso <- function(
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
  
  # Book Applying PSO S.174 (10.2)
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
  vel <- matrix(runif(n*control$s,-1,1), ncol=control$s) * max_wgts * control$maxV
  
  # init particle position
  pos <- matrix(runif(n*control$s,0,1), ncol=control$s) * max_wgts
  
  pos[,1] <- par
  
  # cardinality
  pos <- make_sparse(pos=pos, sum_wgt=control$sum_wgt, cardinal_n=control$cardinal_n, NAV=v$options$NAV, prices=prices)
  
  # fittness
  fit <- apply(pos, 2, fn1)
  
  # init local best
  lb <- pos
  
  # init fittness of all local best
  lb.fit <- fit
  
  # init global best
  gb <- lb[, which.min(lb.fit)]
  
  # init fittness of global best
  gb.fit <- min(lb.fit)
  
  i <- 0
  while(i<=control$maxit){
    i <- i + 1
    
    # new velocity
    vel <- control$inertia * vel + 
      control$c1 * matrix(runif(n*control$s,0,1), ncol=control$s) * ( lb - pos ) +
      control$c2 * matrix(runif(n*control$s,0,1), ncol=control$s) * ( gb - pos )
    
    # new position
    pos <- pos + vel
    
    # cardinality
    pos <- make_sparse(pos=pos, sum_wgt=control$sum_wgt, cardinal_n=control$cardinal_n, NAV=v$options$NAV, prices=prices)
    
    # fittness
    fit <- apply(pos, 2, fn1)
    
    # new local best
    lb[, lb.fit > fit] <- pos[, fit < lb.fit]
    
    # set fittness of all local best
    lb.fit[lb.fit > fit] <- fit[fit < lb.fit]
    
    if(sum(gb.fit > lb.fit)>0){
      # init global best
      gb <- lb[, which.min(lb.fit)]
      
      # init fittness of global best
      gb.fit <- min(lb.fit)
    }
    
    cat(paste0(i,": ", round(gb.fit,4),"  |  "))
  }
  
  return(list("wgt"=gb, "fit"=gb.fit))
}



make_sparse <- function(pos, sum_wgt, cardinal_n, NAV, prices){
  for(k in 1:ncol(pos)){
    pos[order(pos[, k], decreasing = T)[-(1:cardinal_n)], k] <- 0
    pos[, k] <- sum_wgt * pos[, k] / max(sum(pos[, k]), 0.00001)
    pos[, k] <- as.vector(floor(pos[, k]*NAV/coredata(prices)) * coredata(prices) / NAV)
  }
  return(pos)
}



svpso_wrapper <- function(v, save_stats = FALSE){
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
    
    opt <- svpso(
      par = if(!is.null(v$fund$wgts)){v$fund$wgts}else{rep(0, v$pool$assets_n)},
      fn = svpso_obj_func,
      control = list(
        maxit = v$options$iter, # max iterations
        s = 100, # number of particles
        maxV = 0.5, # max velocity
        inertia = 0.5, # inertia weight 
        c1 = 0.5,
        c2 = 0.5,
        cardinal_n = v$constraints$assets_n, # par unequal 0
        max_wgt = v$options$max_wgt,
        sum_wgt = v$constraints$sum_wgts
      ),
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
      "wgts" = opt$wgt,
      "fit" = opt$fit
    )
    
    
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
    
    print("")
    print(paste0("took ", l$time, " secounds"))
    print("___")
  }
  
  
  if(save_stats){
    saved_stats <- bind_rows(env$saved_stats)
    saved_stats <- cbind("row"=1:nrow(saved_stats), saved_stats)
    v$plots$saved_stats <<- saved_stats
  }
  
  
  
}
