#' Summarize data
#' @description
#'
#' For more information about \code{data.table} and \code{setorder()} \href{http://www.google.com}{Google it}.
#'
#' @usage wr_summarise(dt, ...)
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
wr_summarise <- function(dt, ...) {

  # construct a character string of the summary call using data.table syntax
  mk_summarize_call <- function(columnNamesAsString, assignmentsAsString, groupNamesAsString=NULL) {

    byStatement <- NULL

    # no by statement if there's not group in the data.table
    if (!is.null(groupNamesAsString)) {
      byStatement <- paste0("),by=list(",paste(groupNamesAsString, collapse=","))
    }

    paste0("dt[,.(",
           paste(
             paste(columnNamesAsString, assignmentsAsString,sep='='),
             collapse=','),
           byStatement,
           ")]")
  }

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  # catch function call and convert to (named) list
  expr <- as.list(match.call())

  # names in list = names for column/variable to be created
  # store these names as a character vector
  colsAsString <- names(expr)[3:length(names(expr))]

  # extract assignments from the call
  # store these assignments as a character vector
  assignAsString <- lapply(expr[3:length(expr)], function(x)deparse(x)) |> unname() |> unlist()
  #assignAsString <- NULL
  #for (i in 3:length(expr)) {
  #  assignAsString[i-2] <- deparse(expr[[i]])
  #}

  # NULL if input data.table does not contain $group attribute
  groupAttrDT <- attributes(dt)$group
  callAsString <- NULL
  if (is.null(groupAttrDT)) {
    callAsString <- mk_summarize_call(colsAsString, assignAsString)
  } else {
    # column/variable names used for grouping (in the by stsatement)
    groupsAsString <- colnames(groupAttrDT)[-length(colnames(groupAttrDT))] # get groupAttrDT column names
    callAsString <- mk_summarize_call(colsAsString,assignAsString, groupsAsString)
  }
  eval(parse(text=callAsString))
}

## works without groupAttrDTing
#wr_summarise <- function(dt, ...) {
#  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#  expr <- as.list(match.call())
#  colsAsString <- names(expr)[3:length(names(expr))]
#  assignAsString <- list()
#  for (i in 3:length(expr)) {
#    assignAsString[i-2] <- deparse(expr[[i]])
#    argsExpr <- expr[[i]]
#  }
#  callAsString <- paste0("dt[,.(",
#                         paste(
#                           paste(colsAsString, assignAsString,sep='='),
#                           collapse=','),
#                         ")]")
#  eval(parse(text=callAsString))
#  #print(colsAsString)
#  # print(assignAsString)
#  # print(argsExpr)
#  # print(attributes(dt)$groupAttrDT |> colsAsString())
#}
# library(data.table)
# dt <- data.table(mtcars) |> wr.groupAttrDT_by(am)
#
# wr.summarise(dt, mean_mpg = mean(mpg))
#
# dt[,.(mean_mpg=mean(mpg)),by=.(am)]
