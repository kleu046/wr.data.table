mkdt()
test_that("wr_setcols is working", {
  expect_s3_class(wr_setcols(dt, mpg2 = mpg*2), "data.table")
  expect_identical(
    {
      mkdt()
      wr_setcols(dt, mpg2 = mpg*2, vs_am = paste0(vs, am))
    },
    {
      mkdt()
      data.table(dt,dt[, .(mpg2 = mpg*2, vs_am = paste0(vs, am))])
    })
})
rm(dt)


