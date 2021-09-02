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
# wr_groupby <- function(dt, ...) {
#   expr <- match.call()
#   stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#
#   expr <- lapply(expr[3:length(expr)], function(x){
#     if (!as.character(x) %in% colnames(dt)){
#       stop("column/variable does not exist")}
#     as.character(x)})
#
#   attr(dt, "group") <- unlist(expr)
#   dt
# }

# wr_groupby <- function(dt, ...) {
#   expand_colnames <- function(range) {
#     range <- gsub("\"","",range)
#     bracketColNames <- unlist(strsplit(range, ":"))
#     vars <- colnames(dt)
#     start_index <- grep(bracketColNames[[1]], vars)
#     end_index <- grep(bracketColNames[[2]], vars)
#     vars[start_index:end_index]
#   }
#
#   stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#
#   argsAsString <- as.character(as.list(match.call())[-1:-2])
#
#   isRange <- grepl(":", argsAsString)
#
#   rangeAsString <- argsAsString[isRange]
#   colAsString <- argsAsString[!isRange]
#
#   print(rangeAsString)
#   print(colAsString)
#
#   #expr <- match.call()
#   #expr <- expr[-1:-2]
#
#
#   # find columns in range
#   #rangeAsString <- NULL
#   #rangeAsString <- unlist(lapply(expr, function(x) {
#   #  if (grepl(":", deparse(x))){
#   #    deparse(x)
#   #  }}))
#
#   expandedRangeAsString <- lapply(rangeAsString, function(x) {expand_colnames(x)})
#
#   # find individuals
#   #colAsString <- lapply(expr, function(x){
#   #  if (!grepl(":", deparse(x))) {
#   #    if (!as.character(x) %in% colnames(dt)){
#   #      stop("column/variable does not exist")
#   #    }
#   #    as.character(x)
#   #  }
#   #})
#
#   # combine
#   attr(dt, "group") <- unique(c(unlist(expandedRangeAsString), unlist(colAsString)))
#   dt
# }

# select both individuals and ranges with checks
# wr_groupby <- function(dt, ...) {
#
#   stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
#
#   argsAsString <- as.character(as.list(match.call())[-1:-2])
#
#   isRange <- grepl(":", argsAsString)
#
#   # find columns in range
#   if (length(isRange) >= 1) {
#     rangeAsString <- argsAsString[isRange]
#     expandedRangeAsString <- lapply(rangeAsString, function(x) {expand_colnames(dt, x)})
#
#     # conbine
#     attr(dt, "group") <- unique(c(unlist(expandedRangeAsString)))
#   }
#
#   # find individuals columns
#   if (length(isRange) >= 1) {
#     colAsString <- argsAsString[!isRange]
#
#     # combine
#     attr(dt, "group") <- unique(c(attr(dt, "group"), unlist(colAsString)))
#   }
#   dt
# }
