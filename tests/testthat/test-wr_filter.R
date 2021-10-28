mkdt()
test_that("wr_filter is working", {
  expect_s3_class(wr_filter(dt, mpg > 20), "data.table")
  expect_identical(wr_filter(dt, gear ==3 & mpg > 17),
                   dt[gear==3 & mpg>17,])
  expect_error(wr_filter(dt, wrong_col_name > 1))
})
rm(dt)


