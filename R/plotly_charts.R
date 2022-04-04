
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
    to <- if(i != length(v$results)){v$results[[i+1]]$date-1}else{max(index(v$pool$returns))}
    
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
        "TE_train"=round(res$tracking_error_train,6),
        "TE_test"=if(!is.null(res$tracking_error_test)){round(res$tracking_error_test,6)}else{NA},
        "change"=if(is.null(res$percent_change)){"100 %"}else{paste0(round(res$percent_change,4)*100," %")},
        "Alpha"=paste0(round(last(returns$Fund-returns$BM),2), " %"))
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
      text = paste0("TE_train: ", annotation_data$TE_train, "\nTE_test: ", annotation_data$TE_test, "\nAlpha: ", annotation_data$Alpha, "\nchange: ", annotation_data$change),
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


