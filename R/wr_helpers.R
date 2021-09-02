#' @title Helper function to expand a range of columns given with ":" notation
#'
#' @description Given a data.table or data.frame and a string of column range using the ":",
#' expand and find all columns within that range, including the columns stated as the beginning and
#' end of the range
#'
#' @usage expand_colnames(dt, range)
#'
#' @param dt a \code{data.table} or a \code{data.frame}
#' @param range a string defining range of columns using ":" notation
#'
#' @details This helper function is useful for extracting and finding the columns between the bracket
#' of column names provided.  This is useful when evaluating the columns required when other
#' wr_ functions use non-standard evaluation.
#'
#' This function is used internally in the package and not exported
#'
#' @examples \dontrun{
  #' mtcars
  #' expand_colnames(mtcars, "mpg:hp")
#' }
expand_colnames <- function(dt, range) {

  # print(range)
  stopifnot("Input is not using \":\" notation/format." = grepl(":", range))

  brackets <- unlist(strsplit(range, ":"))
  vars <- colnames(dt)
  start_index <- grep(brackets[[1]], vars)
  end_index <- grep(brackets[[2]], vars)
  vars[start_index:end_index]
}

