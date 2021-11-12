#' Arranging rows by selected variables
#' @description Arranging rows of data in the data.table by selected variables
#'   in ascending order. This is similar to the arrange() function in the dplyr
#'   package.
#' @usage ascend_rows(dt, ...)
#' @param dt input data.table
#' @param ... column/variable names in the data.table to be used for ordering
#'   the raws of data
#' @export
ascend_rows <- function(dt, ...) {
  dots <- substitute(list(...))
  if (!is.null(attributes(dt)$group)) {
    copy(dt)[,.SD[order(eval(dots,.SD)),],by=eval(attributes(dt)$group)]
  } else {
    copy(dt)[order(eval(dots,dt)),]
  }
}
