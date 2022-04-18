init_optimizer <- function(pool_prices, bm_wgts=NULL, bm_returns=NULL, rebalance_at=NULL, constraints_assets_n=10, iter=100){
  
  v <- list(
    "pool" = list(
      "returns" = NULL,
      "assets_n" = NULL,
      "prices" = NULL
    ),
    "fund" = list(
      "wgts" = NULL,
      "returns" = NULL
    ),
    "algorithm" = list(
      "pso_pkg" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("return"=1, "risk"=1 ,"sharp_ratio"=1, "sum_wgts"=10, "tracking_error"=5, "assets_n"=10, "change"=10, "short"=10, "beta"=1),
          "tracking_error" = list("reduce_historical_intensity_to"=0.5, "reduce_positivs"=1)
        )
      ),
      "svpso" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("return"=1, "risk"=1 ,"sharp_ratio"=1, "sum_wgts"=10, "tracking_error"=5, "assets_n"=10, "change"=10, "short"=10, "beta"=1),
          "tracking_error" = list("reduce_historical_intensity_to"=0.5, "reduce_positivs"=1)
        )
      ),
      "cvpso" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("return"=1, "risk"=1 ,"sharp_ratio"=1, "sum_wgts"=10, "tracking_error"=5, "assets_n"=10, "change"=10, "short"=10, "beta"=1),
          "tracking_error" = list("reduce_historical_intensity_to"=0.5, "reduce_positivs"=1)
        )
      ),
      "csvpso" = list(
        "fun" = NULL,
        "settings" = list(
          "risk_factor_intensity" = list("return"=1, "risk"=1 ,"sharp_ratio"=1, "sum_wgts"=10, "tracking_error"=5, "assets_n"=10, "change"=10, "short"=10, "beta"=1),
          "tracking_error" = list("reduce_historical_intensity_to"=0.5, "reduce_positivs"=1)
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
      #"round_at" = 6,
      "data_history" = 100, # days
      "NAV" = 10000000,
      #"fee" = 0.001,
      #"min_transaction_size" = 100000,
      "max_wgt" = 0.04
    ),
    "constraints" = list(
      "sum_wgts" = 0.999,
      "assets_n" = NULL,
      "change" = 0.2,
      #"short" = NULL,
      "beta" = 1,
      "transaction_fee" = 0.001
    ),
    "results" = list(
    ),
    "plots" = list(
    )
  )
  
  v$options$iter <- iter
  
  v$pool$prices <- pool_prices %>% 
    pivot_wider(names_from = "ticker", values_from = "price") %>%
    arrange(timestamp) %>%
    data.frame()
  
  v$pool$prices <- xts(v$pool$prices %>% select(-c("timestamp")), order.by=as.Date(v$pool$prices$timestamp))
  
  if(any(colSums(is.na(v$pool$prices)) != 0)){
    message("tickers with NA values in prices are removed. result contains ",sum(colSums(is.na(v$pool$prices)) == 0)," of ", ncol(v$pool$prices)," initial tickers.")
    v$pool$prices <- v$pool$prices[, colSums(is.na(v$pool$prices)) == 0]
  }
  
  v$pool$returns <- v$pool$prices[-1,]/coredata(v$pool$prices[-nrow(v$pool$prices),])-1
  
  v$pool$assets_n <- ncol(v$pool$returns)
  
  
  v$bm$wgts <- if(!is.null(bm_wgts)){
  #   rep(1/ncol(v$pool$returns),ncol(v$pool$returns))
  # }else{
    bm_wgts
  }
  
  v$bm$returns <- if(!is.null(bm_returns)){
    bm_returns
  }else if(!is.null(v$bm$wgts)){
    xts(v$pool$returns %*% rep(1/v$pool$assets_n,v$pool$assets_n), order.by=index(v$pool$returns)) 
  }
  
  
  v$options$rebalance_at <- if(is.null(rebalance_at)){
    max(index(v$pool$returns))
  }else{
    rebalance_at
  }
    
  v$constraints$assets_n <- constraints_assets_n
  
  #v$algorithm$pso_pkg$fun <- pso_pkg_wrapper
  
  v$algorithm$pso_pkg$fun <- pso_pkg_wrapper
  
  v$algorithm$svpso$fun <- svpso_wrapper
  v$algorithm$cvpso$fun <- cvpso_wrapper
  v$algorithm$csvpso$fun <- csvpso_wrapper
  
  return(v)
}