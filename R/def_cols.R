#' Create and assign new columns in data.table
#' @description
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage def_cols(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... expression setting up a new column.  E.g. kpl = mpg / 2.35215.
#' More than one new columns can be created by having expressions separated by commas
#'
#' @return returns a \code{data.table}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
  #' dt <- data.table(mtcars)
  #' def_cols(dt, kmpl = mpg / 2.35215, wt_in_kg = wt / 2.20462)
  #' # creating two columns / converted units
  #' def_cols(dt, am = NULL) # remove a column
#' }
#' @export
def_cols <- function(dt, ...) {
  # dots = named list of ...
  # names = names for new columns
  # values in list = values for assignment

  dots <- substitute(list(...))

  dots <- paste(
    paste(paste0("`",names(dots[-1]),"`"),
          paste0(dots[-1]),
          sep="="),
    collapse=",")

  callAsString <- paste0("copy(dt)[,`:=`(",dots,")]")

  eval(parse(text=callAsString), envir=dt, enclos=parent.frame())
}
