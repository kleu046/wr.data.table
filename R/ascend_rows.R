#' Arranging rows by selected variables
#' @description Arranging rows of data in the data.table by selected variables
#'   in ascending order. This is similar to the arrange() function in the dplyr
#'   package.
#' @usage ascend_rows(dt, ...)
#' @param dt input data.table
#' @param ... column names or a vector of characters of column names in the data.table to be used for ordering
#'   the raws of data
#' @examples
#' \dontrun{
#'  # in descending order by values in weight and mpg
#'  dt <- data1 |> data.table()
#'
#'  dt |> ascend_rows(wt, mpg)
#'
#'  # group using cyl and order by wt and mpg
#'  dt |> set_group(cyl) |> ascend_rows(wt, mpg)
#' }
#' @importFrom data.table setorderv
#' @export
ascend_rows <- function(dt, ...) {

  dots <- NULL
  # try: would not work if ... are symbols
  try({
    dots <- eval(substitute(...))
  }, silent = TRUE)
  # convert symbols into characters
  if (is.null(dots)) {
    dots <- as.character(substitute(list(...)))
    dots <- gsub("`", "", dots)
    dots <- dots[2:length(dots)]
  }

  # with groups
  if (!is.null(attributes(dt)$group)) {
    copy(dt)[,.SD[order(.SD[,dots,with=F]),],by=eval(attributes(dt)$group)]
  # no groups
  } else {
    copy(dt)[order(dt[,dots,with=F]),]
  }
}

