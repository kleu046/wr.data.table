#' Function to subset data.table by removal columns/variables
#'
#' @description This function is similar to \code{}
#'
#' @usage wr_deselect(dt, ...)
#'
#' @import data.table
#'
#' @export
wr_deselect <- function(dt, ...) {
  expr <- match.call()

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  expr <- as.list(expr[-1:-2])

  # isRange
  exprAsStrings <- (lapply(expr, function(x) { deparse(x) }) |> unlist())
  for (s in exprAsStrings) {
    ifelse(grepl(":", s),
           callAsString <- paste0("dt[,!(",s,")]"),
           callAsString <- paste0("dt[,!(\"",s,"\")]"))
    dt <- eval(parse(text=callAsString))
  }
  dt
}

# good code for selecting just one variable/column
# wr_select <- function(dt, ...) {
#   expr <- match.call()
#
#   stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#
#   expr <- as.list(expr[-1:-2])
#
#   constructCall <- as.call(c(
#     as.name("list"),
#     c(expr)
#   ))
#
#   dt[,eval(constructCall)]
#}

# good code to select range and separate columns at the same time
#wr_select <- function(dt, ...) {
#  expr <- match.call()
#
#  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#
#  expr <- as.list(expr[-1:-2])
#
#  # isRange
#  isRange <- lapply(expr, function(x) {grepl(":", deparse(x))}) |> unlist()
#  dt_range <- data.table()
#  if (length(expr[isRange]) >= 1) {
#    rangeExprAsStrings <- (lapply(expr[isRange], function(x) { deparse(x) }) |> unlist())
#    for (s in rangeExprAsStrings) {
#      callAsString <- paste0("dt[,",s,"]")
#      dt_range <- cbind(dt_range,eval(parse(text=callAsString)))
#    }
#  }
#
#  # !isRange
#  dt_separate <- data.table()
#  if (length(expr[!isRange]) >= 1) {
#    constructCall <- as.call(c(
#      as.name("list"),
#      c(expr[!isRange])
#    ))
#
#    dt_separate <- copy(dt)[,eval(constructCall)]
#
#  }
#  cbind(dt_range, dt_separate)
#}
