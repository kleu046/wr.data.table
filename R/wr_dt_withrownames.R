#' Create data.table from data.frame with row names
#' @description This a wrapper function around \code{data.table(df, keep.rownames=TRUE)}
#'
#' For more information about \code{data.table} and \code{data.frame} \href{http://www.google.com}{Google it}.
#'
#' @usage wr_dt_withrownames(df, check.names=FALSE, key = NULL, stringsAsFactors = FALSE)
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
#' wr_dt_withrownames(mtcars)
#'
#' \dontrun{
  #' wr_dt_withrownames(data.table(mtcars)) # this will throw an error because input is not a data.frame
  #' df <- mtcars
  #' rownames(df) <- NULL
  #' wr_dt_withrownames(df)
  #' # if data.frame has no row names, the returned data.table will create column rn with row index numbers
#' }
#'
#' @export wr_dt_withrownames
wr_dt_withrownames <- function(df, check.names=FALSE, key = NULL, stringsAsFactors = FALSE) {
  stopifnot("Input must be a data.frame" = class(df) == "data.frame")

  data.table(rn = rownames(df), df, check.names = check.names, key = key, stringsAsFactors = stringsAsFactors)
}
