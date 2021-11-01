mkdt()
test_that("sel_cols is working", {
  expect_s3_class(sel_cols(dt, mpg), "data.table")

  expect_equal(colnames(sel_cols(dt, vs, mpg:hp, am)),
                   c("vs", "mpg", "cyl", "disp", "hp", "am"))

  expect_identical(ncol(sel_cols(dt, vs, mpg:hp, am, gear:carb )),
                   ncol(dt[,.(vs, mpg,cyl,disp,hp,am,gear,carb)]))

  expect_error(sel_cols(dt, mpg, wrong_col_name, mpg:hp))

  dt1 <- sel_cols(dt, mpg)
  expect_true(!identical(dt1, dt))
  rm(dt1)

})
rm(dt)


