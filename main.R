
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

# load("world_index_data.rdata")
# pool_returns_xts <- daily_return_adjusted_data %>% 
#   select(ticker, timestamp, return) %>%
#   filter(timestamp >= "2018-02-01") %>%  #timestamp <= "2020-06-01"
#   pivot_wider(names_from = "ticker", values_from = "return") %>% 
#   arrange(timestamp) %>% 
#   data.frame()
# pool_returns_xts <- xts(pool_returns_xts[,-1], order.by=as.Date(pool_returns_xts[,1]))
# 
# pool_returns_xts <- pool_returns_xts[, colSums(is.na(pool_returns_xts)) == 0]
# 
# rm(daily_return_adjusted_data)


load("spx_index_data.rdata")
# pool_returns_xts <- spx_daily_return_adjusted_data %>% 
#     select(ticker, timestamp, return_adjust_c) %>%
#     filter(timestamp >= "2017-01-01", timestamp <= "2022-04-07") %>%  #timestamp <= "2020-06-01"
#     pivot_wider(names_from = "ticker", values_from = "return_adjust_c") %>%
#     arrange(timestamp) %>%
#     data.frame()
# pool_returns_xts <- xts(pool_returns_xts[,-1], order.by=as.Date(pool_returns_xts[,1]))
# 
# pool_returns_xts <- pool_returns_xts[, colSums(is.na(pool_returns_xts)) == 0]


pool_prices <- spx_daily_return_adjusted_data %>% 
  select(ticker, timestamp, "price"=adjusted_close) %>%
  filter(timestamp >= "2017-01-01", timestamp <= "2022-04-07")

spx_bm <- spx_bm["2017-01-01/2022-04-07",]

v <- init_optimizer(
  pool_prices = pool_prices,
  bm_returns = spx_bm,
  rebalance_at = seq.Date(from = as.Date(paste0(substr(min(pool_prices$timestamp),1,8),"01")), to=as.Date(paste0(substr(max(pool_prices$timestamp),1,8),"01")), by="months") %>% last(50),
  constraints_assets_n = 50,
  iter = 50
)


v$algorithm$pso_pkg$fun(v = v, save_stats = T)

saved_stats_chart(v$plots$saved_stats, y_max = v$plots$saved_stats$obj %>% mean() * 0.3)

plot_list <- linechart_backtest_returns(v)
plot_list$p_not_split
plot_list$p_split

#save.image("C:\\Users\\Axel\\Desktop\\Master-Thesis-All\\Results_and_Plots\\long_backtest_SPX_22_04_09_v2.rdata")

