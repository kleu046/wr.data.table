#' @title Helper function to expand a range of columns given with ":" notation
#'
#' @description Given a data.table or data.frame and a string of column range
#'   using the ":", expand and find all columns within that range, including the
#'   columns stated as the beginning and end of the range
#'
#' @usage expand_colnames(dt, range)
#'
#' @param dt a \code{data.table} or a \code{data.frame}
#' @param range a string defining range of columns using ":" notation
#'
#' @details This helper function is useful for extracting and finding the
#'   columns between the bracket of column names provided.  This is useful when
#'   evaluating the columns required when other wr_ functions use non-standard
#'   evaluation.
#'
#' This function is used internally in the package and not exported
#'
#' @examples \dontrun{
  #' mtcars
  #' expand_colnames(mtcars, "mpg:hp")
#' }
#' @export
expand_colnames <- function(dt, range) {

  # print(range)
  stopifnot("Input is not using \":\" notation/format." = grepl(":", range))

  brackets <- unlist(strsplit(range, ":"))
  vars <- colnames(dt)
  start_index <- grep(gsub("`", "", brackets[[1]]), vars)
  end_index <- grep(gsub("`", "", brackets[[2]]), vars)
  vars[start_index:end_index]
}

#' @title Helper function to turn characters to symbols
#' @description useful when programming with functions in this package, when
#'   variable names used to store column names as characters are used as
#'   arugments. It is simply a wrapper function for the case of using the
#'   parse() function that convert characters into an unevaluated symbol
#' @usage char_to_symbol(characters)
#' @param characters characters or vector of characters to be turned into symbol
#' @export
char_to_symbol <- function(characters) {
  parse(text=characters)
}
