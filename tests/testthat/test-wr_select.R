mkdt()
test_that("wr_select is working", {
  expect_s3_class(wr_select(dt, mpg), "data.table")
  expect_equal(colnames(wr_select(dt, vs, mpg:hp, am)),
                   c("mpg", "cyl", "disp", "hp", "vs", "am"))
  expect_identical(ncol(wr_select(dt, vs, mpg:hp, am, gear:carb )),
                   ncol(dt[,.(mpg,cyl,disp,hp,vs,am,gear,carb)]))
  expect_error(wr_select(dt, mpg, wrong_col_name, mpg:hp))
})
rm(dt)


