mkdt()
test_that("data.table is created with row names", {
  expect_s3_class(wr_select(dt, mpg), "data.table")
  expect_equal(colnames(wr_select(dt, vs, mpg:hp, am)),
                   c("mpg", "cyl", "disp", "hp", "vs", "am"))
})
rm(dt)
