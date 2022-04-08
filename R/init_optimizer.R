init_optimizer <- function(pool_returns_xts, bm_wgts=NULL, rebalance_at=NULL, constraints_assets_n=10, iter=100){
  
  v <- list(
    "pool" = list(
      "returns" = NULL,
      "assets_n" = NULL,
      "days_n" = NULL
    ),
    "fund" = list(
      "wgts" = NULL,
      "returns" = NULL
    ),
    "algorithm" = list(
      "pso_pkg" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("mean"=10, "sd"=10, "sum_wgts"=10, "tracking_error"=10, "assets_n"=5, "percent_change"=3, "short"=10, "beta"=0),
          "tracking_error" = list("reduce_historical_intensity_to"=0.3, "reduce_positivs"=0.5)
        )
      )
    ),
    "bm" = list(
      "wgts" = NULL,
      "returns" = NULL
    ),
    "options" = list(
      "iter" = 100,
      "rebalance_at" = NULL,
      "round_at" = 6,
      "data_history" = "1y"
    ),
    "constraints" = list(
      "sum_wgts" = 0.999,
      "assets_n" = NULL,
      "percent_change" = 0.2,
      "short" = NULL,
      "beta" = 1,
      "transaction_fee" = 0.001
    ),
    "results" = list(
    ),
    "plots" = list(
    )
  )
  
  v$options$iter <- iter
   
  v$pool$returns <- pool_returns_xts
  v$pool$assets_n <- ncol(v$pool$returns)
  v$pool$days_n <- nrow(v$pool$returns)
  
  
  v$bm$wgts <- if(is.null(bm_wgts)){
    rep(1/ncol(v$pool$returns),ncol(v$pool$returns))
  }else{
    bm_wgts
  }
  
  v$bm$returns <- xts(v$pool$returns %*% rep(1/v$pool$assets_n,v$pool$assets_n), order.by=index(v$pool$returns))
  
  v$options$rebalance_at <- if(is.null(rebalance_at)){
    max(index(v$pool$returns))
  }else{
    rebalance_at
  }
    
  v$constraints$assets_n <- constraints_assets_n
  
  v$algorithm$pso_pkg$fun <- pso_pkg_wrapper
  
  return(v)
}