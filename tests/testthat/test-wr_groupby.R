mkdt()
test_that("wr_groupby is working", {
  expect_identical(attributes(wr_groupby(dt, gear, am, mpg:disp, drat:wt))$group, c("mpg", "cyl", "disp", "drat", "wt", "gear", "am"))
  expect_error(wr_groupby(dt, wrong_col_name))
})
rm(dt)


