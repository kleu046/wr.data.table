mkdt()
test_that("wr_setcols is working", {
  expect_s3_class(wr_setcols(dt, mpg2 = mpg*2), "data.table")
  expect_identical(wr_setcols(dt, mpg2 = mpg*2, vs_am = paste0(vs, am)),
                   data.table(dt,dt[, .(mpg2 = mpg*2, vs_am = paste0(vs, am))]))
  expect_error(wr_setcols(dt, mpg*2))
  #expect_error(wr_setcols(dt, mpg=mpg*1))
  expect_error(wr_setcols(dt, mpg2=mpg*2, mpg2=mpg*2*2))
})
rm(dt)


