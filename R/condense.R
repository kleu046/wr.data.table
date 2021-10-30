#' Summarize data
#' @description
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage condense(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... some parameters
#'
#' @return returns a \code{data.table}
#'
#' @import data.table
#'
#' @examples
#' \dontrun{
#'   dt <- data.table(mtcars)
#' }
#' @export
condense <- function(dt, ...) {

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  args <- substitute(list(...))

  copy(dt)[,eval(args), by=eval(attributes(dt)$group)]

}
