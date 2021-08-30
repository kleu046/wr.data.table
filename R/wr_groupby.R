#' Function to group data by one or more specified variables
#'
#' @import data.table
#'
#' @export
wr_groupby <- function(dt, ...) {
  expr <- match.call()
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))

  expr <- lapply(expr[3:length(expr)], function(x){as.character(x)})

  attr(dt, "group") <- unlist(expr)
  dt
}

