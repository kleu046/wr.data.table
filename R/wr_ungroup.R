#' Ungroup data
#'
#' @description this function simply removes the $group in the \code{data.table} attributes
#'
#' @usage wr_ungroup(dt)
#'
#' @param dt a \code{data.table}
#'
#' @export
wr_ungroup <- function(dt) {
  attributes(dt)$group <- NULL
  dt
}

