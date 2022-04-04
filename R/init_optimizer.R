init_optimizer <- function(pool_returns_xts, bm_wgts=NULL, rebalance_at=NULL, constraints_assets_n=10){
  
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
          "risk_factor_intensity" = list("mean"=1, "sd"=1, "sum_wgts"=1, "tracking_error"=1, "assets_n"=1, "percent_change"=1, "short"=1),
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
      "sum_wgts" = 0.999,
      "assets_n" = NULL,
      "percent_change" = 0.2,
      "short" = NULL
    ),
    "results" = list(
    ),
    "plots" = list(
    )
  )
  
   
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