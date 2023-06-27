.fordyn_tables<-function(x, table_name) {
  l = vector("list", nrow(x))
  for(i in 1:nrow(x)) {
    t_i <- x$result[[i]][[table_name]]
    if(!is.null(t_i)) l[[i]] <- t_i
  }
  df <- data.frame(id = x$id)
  df$table <- l
  return(tidyr::unnest(df, cols="table"))
}