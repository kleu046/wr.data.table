#' Fast row reordering using data.table's order()/setorder function
#' @description This a wrapper function around data.table's row reordering function
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage set_order(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... column names.  Multiple column names are allowed.
#' The function will order the row by the columns in the order they are provided in the arguments
#'
#' @return returns a \code{data.table}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   dt <- data.table(mtcars)
#'   set_order(dt, gear, am, vs)
#' }
#' @export
set_order <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  args <- substitute(list(...))
  eval(call("setorder",dt, args[-1]))

}
