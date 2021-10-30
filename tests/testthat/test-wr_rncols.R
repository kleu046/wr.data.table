mkdt()
test_that("wr_rncols is working", {
  expect_s3_class(wr_rncols(dt, mpg = `miles per gallon`), "data.table")
  expect_identical(
    {
      mkdt()
      wr_rncols(dt, mpg = `miles per gallon`)
    },
    {
      mkdt()
      wr_rncols(dt,"mpg", "miles per gallon")
    })
  expect_identical(
    {
      mkdt()
      wr_rncols(dt, mpg = `miles per gallon`, disp = displacement)
    },
    {
      mkdt()
      wr_rncols(dt,c("mpg", "disp"), c("miles per gallon","displacement"))
    })
})
rm(dt)


