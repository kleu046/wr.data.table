#' Function to subset data.table by selecting columns/variables
#'
#' @description This is a wrapper function to select columns in a data.table
#'
#' @usage sel_cols(dt, ...)
#'
#' @param dt input data.table
#' @param ... vector of characters of column names, column names separated by
#'   commas or range given with ":" notation
#'
#' @details The function creates a copy as data.table would by default.  Columns
#'   will appear in the output data.table in the order that they are given in
#'   "...". Repeat column names are removed.
#'
#' @import data.table
#'
#' @export
sel_cols <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  try({
    if (class(eval(...)) == "character") {
      dots <- eval(...)
      return(copy(dt)[,dots,with=F])
    }
  })

  dots <- as.character(substitute(list(...)))
  dots <- dots[2:length(dots)]

  cols <- NULL
  for (i in 1:length(dots)) {

    if (grepl(":", dots[i])) {
      cols <- c(cols, expand_colnames(dt, dots[i]))
    } else {
      cols <- c(cols, gsub("`", "", dots[i]))
    }
  }

  copy(dt)[,c(unique(cols)),with=F]
}
