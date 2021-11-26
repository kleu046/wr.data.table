mkdt()
test_that("sel_cols is working", {
  expect_s3_class(sel_cols(dt, mpg), "data.table")
  expect_equal(sel_cols(dt, vs, mpg:hp, am),
               copy(dt)[,c("vs", "mpg", "cyl", "disp", "hp", "am")])
  expect_equal(sel_cols(dt, vs, mpg:hp, am, gear:carb ),
               copy(dt)[,c("vs", "mpg","cyl","disp","hp","am","gear","carb")])
  expect_equal(sel_cols(dt, "vs", "am", "gear"),
               copy(dt)[,c("vs","am","gear")])
})
rm(dt)


