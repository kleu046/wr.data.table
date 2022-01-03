mkdt()
var_names <- c("mpg", "drat", "gear")
test_that("desel_cols is working", {
  expect_s3_class(desel_cols(dt, mpg), "data.table")
  expect_equal(colnames(desel_cols(dt, vs, mpg:hp, am)),
                   c("drat", "wt", "qsec", "gear", "carb"))
  expect_equal(desel_cols(dt, vs, mpg:hp, am, gear:carb ),
                   dt[,!c("mpg","cyl","disp","hp","vs","am","gear","carb")])
  expect_equal(desel_cols(dt, "vs", "am", "gear"),
                   dt[,!c("vs","am","gear")])
  expect_equal(desel_cols(dt, c("vs", "am", "gear")),
                   dt[,!c("vs","am","gear")])
  expect_equal(desel_cols(dt, var_names),
               dt[,!c("mpg","drat", "gear")])
})
rm(dt)
rm(var_names)


