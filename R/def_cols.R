#' Create and assign new columns in data.table
#' @description function used to create new columns in data.table using non-standard evaluation
#'
#' @usage def_cols(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... formula using `~` notation where LHS is the new column name and RHS is an expression for the values
#' More than one new columns can be created by having formulas separated by commas
#'
#' @return returns a \code{data.table}
#'
#' @importFrom data.table copy `:=`
#' @importFrom furrr future_map
#' @importFrom rlang caller_env
#'
#' @examples
#' \dontrun{
#' dt <- data.table(mtcars)
#' define(dt, kmpl ~ mpg / 2.35215, wt_in_kg ~ wt / 2.20462)
#' # creating two columns / converted units
#' define(dt, am = NULL) # remove a column
#' }
#' @export
def_cols <- function(dt, ...) {

  env <- caller_env()

  env$dt_copyfromdefcols <- dt

  dots <- substitute(list(...))
  dots <- dots[2:length(dots)]

  nm <- future_map(2:length(dots), function(i) paste0(dots[[i]][[2]]))

  for (i in 1:length(nm)) {
    callasstring <- paste0("dt_copyfromdefcols[,`:=`(",deparse(nm[[i]]),"=",deparse(dots[[i]][[3]]),")]")
    eval(parse(text=callasstring), envir=env)
  }

  return(env$dt_copyfromdefcols)

}
