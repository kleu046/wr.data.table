mkdt()
myvar <- "mpg"
test_that("def_cols is working", {
  expect_s3_class(def_cols(dt, mpg2 = mpg*2), "data.table")
  expect_equal(
      def_cols(dt, mpg2 ~ mpg*2, vs_am ~ paste0(vs, am)),
      dt[, `:=`(mpg2 = mpg*2, vs_am = paste0(vs, am))])
  expect_equal(
    def_cols(dt, `mpg 2` ~ dt[[myvar]] * 2, vs_am ~ paste0(vs, am), `mpg lgl` ~ dt[[myvar]] > 20),
    dt[, `:=`(`mpg 2` = mpg*2, vs_am = paste0(vs, am), "mpg lgl"= mpg > 20)])
})
rm(dt)
rm(myvar)
