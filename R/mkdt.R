#' Function for turning mtcars data.frame into data.table
#'
#' @description for convenience during debugging
#'
#' @return mtcars as \code{data.table}
#'
#' @import data.table
#'
#' @export
mkdt <- function(){
  data.table(mtcars)
}
