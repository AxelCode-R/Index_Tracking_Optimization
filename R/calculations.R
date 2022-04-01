#' calculate continius cummulated returns from returns
#' @export
return_to_cumret <- function(data_xts){
  cumprod(1+rbind.xts(
    xts(matrix(rep(0,ncol(data_xts)), ncol=ncol(data_xts), dimnames = list(NULL, colnames(data_xts))), order.by = min(index(data_xts))-1),
    data_xts
  ))*100
}



