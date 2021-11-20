#' Summarize data
#' @description The condense function is similar to the summarize function in
#'   the dplyr package. It is simply a wrapper function using data.table syntax
#'   which performs a similar function as dplyr::summarize()
#'
#'   Variable names in characters need to be parsed into symbols first
#'
#' @usage condense(dt, ...)
#'
#' @param dt input data.table
#' @param ... some parameters
#'
#' @return returns a \code{data.table}
#'
#' @examples
#' \dontrun{
#'  dt <- data.table(data1)
#'
#'  # using condense function in various ways
#'  dt|>condense(mean_mpg=mean(mpg))
#'  dt|>condense(mean_mpg=mean("mpg"))
#'  dt|>condense(mean_mpg=mean(mpg), sd_mpg=sd(mpg),count=.N)
#'  dt|>condense(mean_mpg=mean("mpg"), sd_mpg=sd(mpg),count=.N)
#'
#'  # grouping data by columns
#'  dt|>set_group("vs","am")|>condense(mean_mpg=mean(mpg))
#'  dt|>set_group(vs,am)|>condense(mean_mpg=mean("mpg"), sd_mpg=sd(mpg),count=.N)
#'  dt|>set_group(vs)|>rn_cols(mpg=`miles per gallon`)|>condense(mean_mpg=mean(`miles per gallon`))
#'
#'  # using a variable to represent the character name of a column
#'  myvar <- "mpg"
#'  dt|>
#'    set_group(vs)|>
#'    condense(mean_mpg=mean(eval(char_to_symbol(myvar))),
#'             sd_mpg=sd(eval(char_to_symbol(myvar))),
#'             max_mpg = max(eval(char_to_symbol(myvar))),
#'             min_mpg = min(eval(char_to_symbol(myvar))),
#'             count=.N)
#' }
#' @export
condense <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  args <- substitute(list(...))
  args <- parse(text=gsub("\"","",deparse(args)))
  copy(dt)[,eval(args), by=eval(attributes(dt)$group)]
}

?try
