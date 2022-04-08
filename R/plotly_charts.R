
#' create linechart for xts cumret data
#' @export
plotly_line_chart_xts <- function(data_xts){
  p <- plot_ly()
  for(i in 1:ncol(data_xts)){
    p <- p %>%
      add_trace(x=as.Date(index(data_xts)), y=coredata(data_xts[,i]), name=if(!is.null(colnames(data_xts)[i])){colnames(data_xts)[i]}else{paste0("trace_",i)}, mode="lines", type = 'scatter')
  }
  p
}



linechart_backtest_returns <- function(v){
  
  all_returns_not_split <- NULL
  all_returns_split <- NULL
  annotation_data <- NULL
  for(i in 1:length(v$results)){
    res <- v$results[[i]]
    
    from <- res$date
    to <- if(i != length(v$results)){v$results[[i+1]]$date-1}else{min(max(index(v$pool$returns)), from + days(30))}
    
    returns <- cbind.xts(
      "Fund" = xts(v$pool$returns[paste0(from,"/",to),] %*% res$wgts, order.by=as.Date(index(v$bm$returns[paste0(from,"/",to),]))),
      "BM" = v$bm$returns[paste0(from,"/",to),]
    )
    
    
    df_returns_not_split <- data.frame("Date"=index(returns), as.data.frame(returns))
    rownames(df_returns_not_split) <- NULL
    
    all_returns_not_split <- bind_rows(
      all_returns_not_split,
      df_returns_not_split
    )
    
    
    returns <- return_to_cumret(returns)
    
    df_returns_split <- data.frame("Date"=c(index(returns),last(index(returns))), rbind(as.data.frame(returns),c(NA,NA)))
    rownames(df_returns_split) <- NULL
    
    all_returns_split <- bind_rows(
      all_returns_split,
      df_returns_split
    )
    
    annotation_data <- bind_rows(
      annotation_data,
      data.frame(
        "Date"=res$date,
        "TE_train"=round(res$te_train,6),
        "TE_train_1i"=round(res$te_train_1i,6),
        "TE_test_1i"=if(!is.null(res$te_test_1i)){round(res$te_test_1i,6)}else{NA},
        "beta_train"=round(res$beta_train,6),
        "beta_train_1i"=round(res$beta_train_1i,6),
        "beta_test_1i"=if(!is.null(res$beta_test_1i)){round(res$beta_test_1i,6)}else{NA},
        "change"=if(is.null(res$percent_change)){"100 %"}else{paste0(round(res$percent_change,4)*100," %")},
        "alpha"=paste0(round(last(returns$Fund-returns$BM),2), " %"))
    )
    
  }
  
  all_returns_not_split <- return_to_cumret(xts(all_returns_not_split[,-1], order.by=as.Date(all_returns_not_split$Date)))
  p_not_split <- plotly_line_chart_xts(all_returns_not_split)
  
  
  
  p_split <- plot_ly() %>%
    add_trace(x=all_returns_split$Date, y=all_returns_split$Fund, name="Fund", mode="lines", type = 'scatter') %>%
    add_trace(x=all_returns_split$Date, y=all_returns_split$BM, name="BM", mode="lines", type = 'scatter') %>%
    add_annotations(
      x = annotation_data$Date,
      y = max(all_returns_split[,2:3], na.rm = T)*1.1,
      text = paste0("TE_train: ", annotation_data$TE_train, 
                    "\nTE_train_1i: ", annotation_data$TE_train_1i, 
                    "\nTE_test_1i: ", annotation_data$TE_test_1i, 
                    "\nbeta_train: ", annotation_data$beta_train, 
                    "\nbeta_train_1i: ", annotation_data$beta_train_1i, 
                    "\nbeta_test_1i: ", annotation_data$beta_test_1i, 
                    "\nalpha: ", annotation_data$alpha, 
                    "\nchange: ", annotation_data$change),
      xref = "x",
      yref = "y",
      showarrow=F) %>%
    layout(
      xaxis = list(
        type = 'date',
        tickformat = "%Y-%m-%d",
        tickvals=c(annotation_data$Date, max(all_returns_split$Date))
      )
    )
  
  
  return(list("p_not_split"=p_not_split, "p_split"=p_split))
  
}



saved_stats_chart <- function(saved_stats, y_max = 1){
  saved_stats <- saved_stats %>%
    mutate(row = round(row/100)) %>%
    group_by(row) %>%
    summarise(return = sum(return),
              risk = sum(risk),
              sum_wgts = sum(sum_wgts),
              te = sum(te),
              asset_n = sum(asset_n),
              change = sum(change),
              short = sum(short),
              obj = sum(obj),
              beta = sum(beta)
    ) %>%
    ungroup() %>%
    mutate(obj = obj/max(obj)*100)

  p1 <- plot_ly(data = saved_stats, x=~row, y=~return, name="return", mode="none", type = 'scatter', stackgroup="one", groupnorm="percent") %>%
    add_trace(y=~risk, name="risk") %>%
    add_trace(y=~sum_wgts, name="sum_wgts") %>%
    add_trace(y=~te, name="te") %>%
    add_trace(y=~asset_n, name="asset_n") %>%
    add_trace(y=~change, name="change") %>%
    add_trace(y=~short, name="short") %>%
    add_trace(y=~beta, name="beta")


  p2 <- plot_ly(data = saved_stats, x=~row, y=~obj, name="obj", mode="lines", type = 'scatter') %>%
    layout(yaxis=list(range=c(0,y_max), ticksuffix="%"))

  return(subplot(p1, p2, nrows = 2))
}
