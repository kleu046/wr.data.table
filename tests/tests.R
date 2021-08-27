#testScript
library(wr.data.table)
library(data.table)
require(testthat)

# data.table.withRownames()
expect_that(data.table.withRownames(mtcars, check.names=TRUE),
            is_identical_to(
              cbind(rn=rownames(mtcars), data.table(mtcars))))

expect_that(data.table.withRownames(mtcars, check.names=TRUE),
            is_a("data.table"))

# expect_match("mtcars", "[[:alnum:]]")
#
# expect_output(paste("something"))
# expect_output(data.table.withRownames(mtcars))
#
# expect_message(data.table.withRownames(mtcars))
#
# expect_warning(data.table.withRownames(mtcars))
#
# expect_error(data.table.withRownames(mtcars))
#
# expect_true(111 > 11111)

# wr.order()
expect_that(
  data.table(mtcars) |> wr.order(gear, am, vs),
  is_identical_to(data.table(mtcars)[order(gear,am,vs)])
)

# wr.filter()
expect_that(
  data.table(mtcars) |> wr.filter(mpg > 20 & am == 1),
  is_identical_to(data.table(mtcars)[mpg > 20 & am == 1])
)

# wr.assignCols()
dt <- data.table(mtcars) |> wr.assignCols(newCol = paste(mpg, carb))
dt

dt <- data.table(mtcars) |> wr.assignCols(kmpl = mpg * 0.425144) # convert to km per L
dt

dt <- data.table(mtcars) |> wr.assignCols(hp = NULL) # delete column
dt

# wr.select()
data.table(mtcars) |> wr.select(mpg)
data.table(mtcars) |> wr.select(mpg, vs)
data.table(mtcars) |> wr.select(mpg:hp, vs,am, mpg, hp)
