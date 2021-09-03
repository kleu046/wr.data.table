#' Function to filter rows of data by condition
#'
#' @description wrapper function around filtering data.table with given conditions with non-standard evaluation
#'
#' @usage wr_filter(dt, condition)
#'
#' @param dt input \code{data.table}
#' @param condition a valid R conditional expression
#'
#' @details This function simply takes the input conditions
#'
#' @examples \dontrun{
  #' wr_filter(data.table(mtcars),mpg > 20)  # return all rows where mpg is > 20
  #' wr_filter(data.table(mtcars), mpg > 20, vs = 1, am = 0)
  #' # return all rows where mpg is > 20, vs = 1, am = 0
  #'
  #' wr.filter(data.table(mtcars), "mpg > 20") # error - conditions should not be parsed as characters
#' }
#'
#' @import data.table
#'
#' @export
wr_filter <- function(dt, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, dt, enclos=parent.frame())
  copy(dt)[r,]
}
