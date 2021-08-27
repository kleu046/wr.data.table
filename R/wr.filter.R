#' Function to filter rows of data by condition
#'
#' @import data.table
#'
#' @export
wr.filter <- function(dt, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, dt, enclos=parent.frame())
  dt[r,]
}
