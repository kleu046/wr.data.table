#' Short to long data
#' @importFrom data.table melt
#' @usage gather(dt, key, ...)
#' @param dt data.table
#' @param key key
#' @param ... ...
#' @export

#temp <- dcast(dt, cyl + disp + hp + drat + wt + qsec + vs + am + carb ~ gear, value.var = "mpg")
#melt(temp, id = c("cyl", "disp"), measure.vars = c("3", "4", "5"))

gather <- function(dt, key, ...) {

}
