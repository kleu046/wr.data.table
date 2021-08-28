#' Create and assign new columns in data.table
#' @description
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage wr_assigncols(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... expression setting up a new column.  E.g. kmpl = mpg / 2.35215.
#' More than one new columns can be created by having expressions separated by commas
#'
#' @return returns a \code{data.table}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
  #' dt <- data.table(mtcars)
  #' wr_assigncols(dt, kmpl = mpg / 2.35215, wt_in_kg = wt / 2.20462) # creating two columns / converted units
  #' wr_assigncols(dt, am = NULL) # remove a column
#' }
#' @export
wr_assigncols <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
  expr <- as.list(match.call())
  colNames <- names(expr)[3:length(names(expr))]
  argsAsString <- list()
  for (i in 3:length(expr)) {
    argsAsString[i-2] <- deparse(expr[[i]])
    argsExpr <- expr[[i]]
  }
  callAsString <- paste0("dt[,`:=`(",
                         paste(
                           paste(colNames, argsAsString,sep='='),
                           collapse=','),
                        ")]")
  eval(parse(text=callAsString))
}
