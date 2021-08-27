#' Create data.table from data.frame with row names
#' @description This a wrapper function around \code{data.table(df, keep.rownames=TRUE)}
#'
#' For more information about \code{data.table} and \code{data.frame} \href{http://www.google.com}{Google it}.
#'
#' @usage data.table.withRownames(df, check.names=FALSE, key = NULL, stringsAsFactors = FALSE)
#'
#' @param df a \code{data.frame}
#' @param check.names same as in data.frame
#' @param key character vector of one or more column names which is passed to setkey
#' @param stringsAsFactors same as in data.table
#'
#' @return returns a \code{data.table}
#'
#' @import data.table
#'
#' @examples
#' data.table.withRownames(mtcars)
#'
#' \dontrun{
  #' data.table.withRownames(classesOtherThanDataFrame)
#' }
#'
#' @export data.table.withRownames
data.table.withRownames <- function(df, check.names=FALSE, key = NULL, stringsAsFactors = FALSE) {
  stopifnot("Input must be a data.frame" = class(df) == "data.frame")

  data.table(rn = rownames(df), df, check.names = check.names, key = key, stringsAsFactors = stringsAsFactors)
}
