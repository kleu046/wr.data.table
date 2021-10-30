mkdt()
test_that("filter_rows is working", {
  expect_s3_class(filter_rows(dt, mpg > 20), "data.table")
  expect_identical(filter_rows(dt, gear ==3 & mpg > 17),
                   dt[gear==3 & mpg>17,])
  expect_error(filter_rows(dt, wrong_col_name > 1))
})
rm(dt)


