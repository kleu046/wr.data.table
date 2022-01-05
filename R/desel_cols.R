#' Function to subset data.table by removal columns/variables
#'
#' @description This function is similar to sel_cols and used to deselect
#'   columns in a data.table
#'
#' @usage desel_cols(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... characters or vector of characters of column names or column names as symbols
#'
#' @export
desel_cols <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  try({
    if (class(eval(...)) == "character") {
      dots <- eval(...)
      return(copy(dt)[,!dots,with=F])
    }
  }, silent = TRUE)

  args <- (substitute(list(...)))

  argsAsString <- lapply(args[-1], as.character)

  isRange <- grepl(":", argsAsString)

  argsAsStringIsRange <- lapply(argsAsString[isRange], function(x){
      paste0(x[2], ":", x[3])
  })

  cols <- c(
    sapply(argsAsStringIsRange, function(x) {expand_colnames(dt, x)}) |> unlist(),
    argsAsString[!isRange] |> unlist())

  # remove "c" if vector of characters of column names were provided
  if (cols[1] == "c") {
    cols <- cols[-1]
  }
  #}

  copy(dt)[,!c(cols),with=F]
}
