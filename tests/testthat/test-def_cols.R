mkdt()
test_that("def_cols is working", {
  expect_s3_class(def_cols(dt, mpg2 = mpg*2), "data.table")
  expect_identical(
      def_cols(dt, mpg2 = mpg*2, vs_am = paste0(vs, am)),
      data.table(dt,dt[, .(mpg2 = mpg*2, vs_am = paste0(vs, am))]))
})
rm(dt)


