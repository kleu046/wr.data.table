#' Summarize data
#' @description
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage wr.summarise(dt, ...)
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
wr.summarise <- function(dt, ...) {
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
  #print(colNames)
  # print(argsAsString)
  # print(argsExpr)
  # print(attributes(dt)$group |> colnames())
}

mtcars[1,]

# library(data.table)
# dt <- data.table(mtcars) |> wr.group_by(am)
#
# wr.summarise(dt, mean_mpg = mean(mpg))
#
# dt[,.(mean_mpg=mean(mpg)),by=.(am)]
