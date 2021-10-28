#' Function for turning mtcars data.frame into data.table
#'
#' @description for convenience during debugging.  Creates a data.table called \code{dt} in the global environment
#'
#' @usage mkdt()
#'
#' @import data.table
#'
#' @export
mkdt <- function(){
  eval(quote(dt <- data.table::data.table(mtcars)), envir=parent.frame())
}
