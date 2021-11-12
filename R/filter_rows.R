#' Function to filter rows of data by condition
#'
#' @description wrapper function around filtering data.table with given conditions with non-standard evaluation
#'
#' @usage filter_rows(dt, condition)
#'
#' @param dt input \code{data.table}
#' @param condition a valid R conditional expression
#'
#' @details This function simply takes the input conditions
#'
#' @examples \dontrun{
  #' filter_rows(data.table(mtcars),mpg > 20)  # return all rows where mpg is > 20
  #' filter_rows(data.table(mtcars), mpg > 20, vs = 1, am = 0)
  #' # return all rows where mpg is > 20, vs = 1, am = 0
  #'
  #' wr.filter(data.table(mtcars), "mpg > 20") # error - conditions should not be parsed as characters
#' }
#'
#' @import data.table
#'
#' @export
filter_rows <- function(dt, condition) {
  condition_call <- substitute(condition)
  if (!is.null(attributes(dt)$group)) {
    copy(dt)[,.SD[eval(condition_call, .SD),], by=eval(attributes(dt)$group)]
  } else {
    copy(dt)[eval(condition_call, dt, enclos=parent.frame()), ]
  }
}
