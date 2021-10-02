mkdt()
test_that("wr_deselect is working", {
  expect_s3_class(wr_deselect(dt, mpg), "data.table")
  expect_equal(colnames(wr_deselect(dt, vs, mpg:hp, am)),
                   c("drat", "wt", "qsec", "gear", "carb"))
  expect_identical(ncol(wr_deselect(dt, vs, mpg:hp, am, gear:carb )),
                   ncol(dt[,!c("mpg","cyl","disp","hp","vs","am","gear","carb")]))
  #expect_error(wr_deselect(dt, mpg, wrong_col_name, mpg:hp))
})
rm(dt)


