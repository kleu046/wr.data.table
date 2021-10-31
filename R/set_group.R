#' Group data using variables
#'
#' @description add $group attribute to data.table
#' $group is a vector of variable anmes
#'
#' @usage set_group(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... names of variables that are used for grouping
#'
#' @import data.table
#'
#' @export
set_group <- function(dt, ...) {

  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  argsAsString <- as.character(as.list(match.call())[-1:-2])

  isRange <- grepl(":", argsAsString)

  # find columns in range
  rangeAsString <- argsAsString[isRange]
  expandedRangeAsString <- sapply(rangeAsString, function(x) {expand_colnames(dt, x)})

  # find individual columns
  colAsString <- argsAsString[!isRange]

  # find column names that has quotes
  hasQuote <- grepl("^`.*`$", colAsString)

  colAsStringHasQuote <- gsub("`", "", colAsString[hasQuote])

  colAsStringNoQuote <- colAsString[!hasQuote]

  combinedColsAsString <- unique(c(unlist(expandedRangeAsString), unlist(colAsStringHasQuote), unlist(colAsStringNoQuote)))

  # combine
  if(!all(combinedColsAsString %in% colnames(dt))) {
    stop("one or more column names in ... does not exist")
  } else {
    attr(dt, "group") <- combinedColsAsString
    dt
  }
}
