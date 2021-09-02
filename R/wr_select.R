#' Function to subset data.table with selected columns/variables
#'
#' @description This function is used in similar way as \code{dplyr::select()}.
#' The column names for required columns are provided as expressions in \code{...}
#' to select the desired columns in a data.table.
#'
#' @usage wr_select(dt, ...)
#'
#' @param dt a data.table
#' @param ... column names, such as \code{mpg} or \code{drat}
#' will select the columns mpg and drat in mtcars; or
#' columns name as a range, such as \code{mpg:hp} will select all columns between
#' mpg and hp in mt cars
#'
#' @details The function selects the columns as stated in \code{...}.
#' A series of columns can selected in one go.  They can also be given as a range using
#' ":" notation or as individual column names.
#'
#' Do not input the column names as characters.
#'
#' Different than \code{dplyr::select()} where user can select the data.frame / tibble
#' with the column prefixed with "-", this function is only used for selecting
#' columns/range of columns stated explicitly.  The function \code{\link[wr.data.table]{wr_deselect}} is
#' used instead to perform "negative select".
#'
#' column selected by range always appear in first in the \code{data.table} (on the left-hand side)
#'
#' if column range is given in reverse order, the columns will appear in reverse order
#'
#' @return a \code{data.table}
#'
#' @examples \dontrun{
  #' dt <- data.table(mtcars)
  #'
#' }
#'
#' @import data.table
#'
#' @export
wr_select <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  argsAsString <- as.character(as.list(match.call())[-1:-2])

  # isRange
  isRange <- unlist(lapply(argsAsString, function(x) {grepl(":", x)}))

  rangeAsString <- argsAsString[isRange]
  expandedRangeAsString <- lapply(rangeAsString, function(x) {expand_colnames(dt, x)})
  expandedRangeAsString <- unlist(expandedRangeAsString)

  # !isRange
  individualAsString <- argsAsString[!isRange]

  # combine
  combinedColsAsString <- unique(c(expandedRangeAsString, individualAsString))

  if(!all(combinedColsAsString %in% colnames(dt))) {
    stop("one or more column names in ... does not exist")
  } else {
    copy(dt)[,combinedColsAsString,with=F]
  }
}
