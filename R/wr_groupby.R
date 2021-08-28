#' Function to group data by one or more specified variables
#'
#' @import data.table
#'
#' @export
wr_groupby <- function(dt, ...) {
  expr <- match.call()
  stopifnot("dt must be data.table" = any(class(dt) == "data.table"))
  expr <- lapply(expr[3:length(expr)], function(x){x})

  exprAsString <- deparse(expr)
  expr <- parse(text=exprAsString)

  group_dt <- dt[,.I, by=eval(expr)][,list(I=list(I)), by=eval(expr)]
  attr(dt, "group") <- group_dt
  dt
}

