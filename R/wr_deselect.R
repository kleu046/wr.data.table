#' Function to subset data.table by removal columns/variables
#'
#' @description This function is similar to
#'
#' @usage wr_deselect(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... some parameters
#'
#' @import data.table
#'
#' @export
wr_deselect <- function(dt, ...) {
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
    copy(dt)[,-combinedColsAsString,with=F]
  }
}

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
