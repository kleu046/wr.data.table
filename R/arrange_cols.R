#' Arrange columns
#' @description This wrapper function make use of the setcolorder() function and
#'   arrange columns by placing selected columns after the specified position.
#' @usage arrange_cols(dt, at, ...)
#' @param dt input data.table
#' @param at position for inserting the selected columns.  This can be any valid
#'   column names, which will place the selected columns after the "at" column.
#'   Alternatively use "start" or "end" for placing the selected columns before
#'   the first column or after the last column respectively
#' @param ... a vector of characters of column names or valid column names seperated
#'   by commas.
#' @examples
#' \dontrun{
#' arrange_cols(dt, at=`vehicle model`, qsec, vs, am)
#' arrange_cols(dt, at="start", c("qsec", "vs", "am"))
#' arrange_cols(dt, at="mpg", qsec:am, carb)
#' }
#' @importFrom data.table setcolorder
#' @export
arrange_cols <- function(dt, at, ...) {
  if(is.symbol(substitute(at))) {
    at <- paste0(substitute(at))
  }
  stopifnot("at number be a valid column name, \"start\" or \"end\"" = at %in% colnames(dt)|(at=="start")|(at == "end"))

  dots <- NULL
  try({
    dots <- eval(substitute(...))
  }, silent = T)
  if (is.null(dots)) {
    dots <- as.character(substitute(list(...)))
    dots <- gsub("`", "", dots)
    dots <- dots[2:length(dots)]
  }

  # expand range of column names
  if (any(grepl(":", dots))) {
    extract_cols <- NULL
    for (i in 1:length(dots)) {
      if(grepl(":", dots[i])) {
        expanded <- expand_colnames(dt, dots[i])
        extract_cols <- c(extract_cols, expanded)
      } else {
        extract_cols <- c(extract_cols, dots[i])
      }
    }
  } else {
    extract_cols <- dots
  }

  all_cols <- colnames(dt)
  remain_cols <- all_cols[!grepl(paste0(extract_cols,collapse="|"), all_cols)]

  if(at == "start") {
    colorder <- c(extract_cols, remain_cols)
  } else if (at == "end") {
    colorder <- c(remain_cols, extract_cols)
  } else {
    insert_index <- grep(at, remain_cols)
    if (insert_index == length(remain_cols)) {
      colorder <- c(remain_cols, extract_cols)
    } else {
      colorder <- c(remain_cols[1:insert_index], extract_cols, remain_cols[(insert_index+1):length(remain_cols)])
    }
  }
  setcolorder(copy(dt), colorder)
}
