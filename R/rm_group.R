#' Ungroup data
#'
#' @description this function simply removes the $group in the \code{data.table} attributes
#'
#' @usage rm_group(dt)
#'
#' @param dt a \code{data.table}
#'
#' @export
rm_group <- function(dt) {
  attributes(dt)$group <- NULL
  dt
}

