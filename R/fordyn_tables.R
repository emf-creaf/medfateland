# merges summary tables
fordyn_tables<-function(x, table_name) {
  l = vector("list", nrow(x))
  for(i in 1:nrow(x)) l[[i]] = x$result[[i]][[table_name]]
  df = data.frame(id = x$id)
  df$table = l
  return(tidyr::unnest(df, cols="table"))
}