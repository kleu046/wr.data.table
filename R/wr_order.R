#' Fast row reordering using data.table's order()/setorder function
#' @description This a wrapper function around data.table's row reordering function
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage wr_order(dt, ...)
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
#'   wr_order(dt, gear, am, vs)
#' }
#' @export
wr_order <- function(dt, ...) {
  expr <- match.call()
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
  expr <- lapply(expr[3:length(expr)], function(x){x})
  constructCall <- as.call(c(
    as.name("setorder"),
    c(quote(copy(dt)),expr)
  ))
  eval(constructCall)
}

#wr_order <- function(dt, ...) {
#  expr <- match.call()
#  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#  argsAsString <- list()
#  for (i in 3:length(expr)) {
#    argsAsString[i-2] <- deparse(expr[[i]])
#  }
#  callAsString <- paste0("dt[order(",paste(argsAsString, collapse=","),")]")
#  eval(parse(text=callAsString))
#}


