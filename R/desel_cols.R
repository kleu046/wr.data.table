#' Function to subset data.table by removal columns/variables
#'
#' @description This function is similar to
#'
#' @usage desel_cols(dt, ...)
#'
#' @param dt a \code{data.table}
#' @param ... some parameters
#'
#' @import data.table
#'
#' @export
desel_cols <- function(dt, ...) {
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  args <- (substitute(list(...)))

  argsAsString <- lapply(args[-1], as.character)

  isRange <- grepl(":", argsAsString)

  argsAsStringIsRange <- lapply(argsAsString[isRange], function(x){
      paste0(x[2], ":", x[3])
  })
  cols <- c(
    sapply(argsAsStringIsRange, function(x) {expand_colnames(dt, x)}) |> unlist(),
    argsAsString[!isRange] |> unlist())

  copy(dt)[,!c(cols),with=F]
}
