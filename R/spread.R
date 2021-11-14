#' long to short data
#' @usage spread(dt, key, value)
#' @param dt input data.table
#' @param key key variable used to spread data
#' @param value data values for new columns
#' @importFrom data.table dcast
#' @export
spread <- function(dt, key, value) {
  key <- paste0("`",deparse(substitute(key)),"`")
  value <- deparse(substitute(value))
  SDcols <- paste0("`",colnames(dt),"`")
  SDcols <- SDcols[c(-grep(key, SDcols),-grep(paste0("`",value,"`"), SDcols))]
  SDcols <- paste(paste(SDcols, collapse="+"))
  eval(parse(text=paste0("dcast(dt,",SDcols,"~",key,",value.var=\"",value,"\")")))
}

