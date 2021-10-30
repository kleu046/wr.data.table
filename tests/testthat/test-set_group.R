mkdt()
test_that("set_group is working", {
  expect_identical(attributes(set_group(dt, gear, am, mpg:disp, drat:wt))$group, c("mpg", "cyl", "disp", "drat", "wt", "gear", "am"))
  expect_error(set_group(dt, wrong_col_name))
})
rm(dt)


