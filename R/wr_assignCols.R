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
  #' wr_assigncols(dt, kmpl = mpg / 2.35215, wt_in_kg = wt / 2.20462)
  #' # creating two columns / converted units
  #' wr_assigncols(dt, am = NULL) # remove a column
#' }
#' @export
wr_assigncols <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  # named list
  # names = new column names (to be created)
  # values in list = values for assignment
  args <- as.list(match.call())[-1:-2]

  newColNames <- names(args)

  stopifnot("column names must be unique" =
              length(unique(c(colnames(dt),newColNames))) == (length(colnames(dt)) + length(newColNames)))

  callAsString <- paste0("copy(dt)[,`:=`(",
                         paste(
                           paste(newColNames, args,sep='='),
                           collapse=','),
                        ")]")
  eval(parse(text=callAsString))
}
