#' Setting column names
#' @description using data.table packages setnames function to change column
#'   names
#' @usage rn_cols(dt, old=NULL, new=NULL, ...)
#' @param dt input data.table
#' @param old default = NULL vector of existing column names
#' @param new default = NULL vector of new column names
#' @param ... non-standard evaluation of in the form of an expression such as,
#'   old_column_name = new_column_name
#' @details User can use provide a vector of characters for current column names
#'   and a vector of characters of equal lengths as new column names.
#'   Alternatively, user can also use the old_column_name=new_column_name
#'   notation, separated by comma.
#' @examples
#' \dontrun{
#' rn_cols(dt, hp=horsepower, disp=displacement)
#' rn_cols(dt, old=c("hp", "disp", "cyl"), new=c("horsepower", "displacement", "cylinder"))
#' }
#' @importFrom data.table setnames
#' @export
rn_cols <- function(dt, old=NULL, new=NULL, ...){

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
