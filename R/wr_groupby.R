#' Group data using variables
#'
#' @description add $group attribute to data.table
#' $group is a vector of variable anmes
#'
#' @usage wr_groupby(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... names of variables that are used for grouping
#'
#' @import data.table
#'
#' @export
wr_groupby <- function(dt, ...) {

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  argsAsString <- as.character(as.list(match.call())[-1:-2])

  isRange <- grepl(":", argsAsString)

  # find columns in range
  rangeAsString <- argsAsString[isRange]
  expandedRangeAsString <- lapply(rangeAsString, function(x) {expand_colnames(dt, x)})

  # find individual columns
  colAsString <- argsAsString[!isRange]

  combinedColsAsString <- unique(c(unlist(expandedRangeAsString), unlist(colAsString)))

  # combine
  if(!all(combinedColsAsString %in% colnames(dt))) {
    stop("one or more column names in ... does not exist")
  } else {
    attr(dt, "group") <- combinedColsAsString
    dt
  }
}
