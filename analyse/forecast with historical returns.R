

x <- daily_return_adjusted_data %>% 
  mutate(t2=substr(timestamp,1,7)) %>%  
  group_by(t2, ticker) %>% 
  summarise(mean_ret = mean(return))

x <- x %>% arrange(ticker, t2) %>%  mutate(mean_ret_prev = lag(mean_ret))

lm(x$mean_ret ~ x$mean_ret_prev)

plot_ly() %>%
  add_trace(x=x$mean_ret, y=x$mean_ret_prev, name="a", mode="scatter", type = 'scatter')

plot(x=x$mean_ret, y=x$mean_ret_prev)
abline(lm(x$mean_ret ~ x$mean_ret_prev))
