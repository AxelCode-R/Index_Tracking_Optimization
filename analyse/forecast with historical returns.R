

x <- spx_daily_return_adjusted_data %>% 
  mutate(t2=substr(timestamp,1,7)) %>%  
  group_by(t2, ticker) %>% 
  summarise(mean_ret = mean(return_adjust_c), sd_ret = sd(return_adjust_c)) %>% 
  ungroup() %>% 
  mutate(sharpe_ratio = mean_ret/sd_ret)
#rm(daily_return_adjusted_data)

x <- x %>% arrange(ticker, t2) %>%  mutate(sharpe_ratio_prev = lag(sharpe_ratio)) 
x <- x %>% filter(!is.na(sharpe_ratio_prev), !is.na(sharpe_ratio))

lm(x$sharpe_ratio ~ x$sharpe_ratio_prev)

plot_ly() %>%
  add_trace(x=x$mean_ret, y=x$mean_ret_prev, name="a", mode="scatter", type = 'scatter')

plot(x=x$mean_ret, y=x$mean_ret_prev)
abline(lm(x$mean_ret ~ x$mean_ret_prev))
 



bm <- daily_return_adjusted_data %>% 
  #filter(volume > 1000000) %>% 
  group_by(timestamp) %>% 
  summarise(return = mean(return)) 

plotly_line_chart_xts(return_to_cumret(xts(bm$return, order.by=bm$timestamp)))





