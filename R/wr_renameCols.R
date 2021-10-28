#' importFrom data.table setnames
#' @export
wr_renameCols <- function(dt, old=NULL, new=NULL, ...){
  if (is.null(old) & is.null(new)) {
    dots <- substitute(list(...))
    col_names <- names(dots)
    for (i in 2:length(dots)) {
      setnames(dt, toString(col_names[i]), toString(dots[[i]]))
    }
  } else {
    stopifnot(exprs = {
      length(old) == length(new)
      is.character(old)
      is.character(new)
      old %in% colnames(dt)
    })
    for (i in 1:length(old)) {
      setnames(dt, old[i], new[i])
    }
  }
  dt
}
