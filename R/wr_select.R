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
#' Different the \code{dplyr::select()} where user can select the data.frame / tibble
#' with the column prefixed with "-", this function is only used for selecting
#' columns/range of columns stated explicitly.  The function \link[wr.data.table]{wr_deselect} is
#' used instead to perform "negative select".
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
  expr <- match.call()

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  expr <- as.list(expr[-1:-2])

  # isRange
  isRange <- NULL
  isRange <- lapply(expr, function(x) {grepl(":", deparse(x))}) |> unlist()
  dt_range <- data.table()
  if (length(expr[isRange]) >= 1) {
    rangeExprAsStrings <- (lapply(expr[isRange], function(x) { deparse(x) }) |> unlist())
    for (s in rangeExprAsStrings) {
      callAsString <- paste0("copy(dt)[,",s,"]")
      dt_range <- cbind(dt_range,eval(parse(text=callAsString)))
    }
  }

  # !is_Range
  dt_separate <- data.table()
  if (length(expr[!isRange]) >= 1) {
    constructCall <- as.call(c(as.name("list"),c(expr[!isRange])))

    dt_separate <- copy(dt)[,eval(constructCall)]

  }
  dt_result <- cbind(dt_range, dt_separate)
  if (length(unique(colnames(dt_result))) < length(dt_result)) {
     warning("Non-unique column/variable names in returned data.table")
  }
  dt_result
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
