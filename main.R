
# SETTINGS
library(dplyr)
library(xts)
library(lubridate)
library(pso)
library(plotly)
library(tidyr)

for(src in list.files("R")){
  source(paste0("R/", src))
}

get_global()



# GET DATA
# load("R-research/returns_df.rdata")
# pool_returns_df <- returns_df
# rownames(pool_returns_df) <- seq.Date(from = Sys.Date()-nrow(pool_returns_df)+1, to=Sys.Date(), by="days")
# pool_returns_xts <- xts(pool_returns_df, order.by = as.Date(rownames(pool_returns_df)))

load("world_index_data.rdata")
pool_returns_xts <- daily_return_adjusted_data %>% 
  select(ticker, timestamp, return) %>%
  filter(timestamp >= "2020-01-01") %>% 
  pivot_wider(names_from = "ticker", values_from = "return") %>% 
  arrange(timestamp) %>% 
  data.frame()
pool_returns_xts <- xts(pool_returns_xts[,-1], order.by=as.Date(pool_returns_xts[,1]))

pool_returns_xts <- pool_returns_xts[, colSums(is.na(pool_returns_xts)) == 0]

rm(daily_return_adjusted_data)

v <- init_optimizer(
  pool_returns_xts = pool_returns_xts,
  rebalance_at = seq.Date(from = as.Date(paste0(substr(min(index(pool_returns_xts)),1,8),"01")), to=as.Date(paste0(substr(max(index(pool_returns_xts)),1,8),"01")), by="months") %>% last(20),
  constraints_assets_n = 100
)



v <- v$algorithm$pso_pkg$fun(v = v, save_stats = F)


v$plots$saved_stats_plot

plot_list <- linechart_backtest_returns(v)
plot_list$p_not_split
plot_list$p_split



