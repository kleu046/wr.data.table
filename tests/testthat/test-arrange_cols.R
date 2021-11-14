mkdt()
dt1 <- data1 |> dt_keeprownames() |> rn_cols(rn=`vehicle model`)
test_that("arrange_cols is working", {
  expect_equal(arrange_cols(dt, at="start", vs), data.table::setcolorder(copy(dt), c("vs", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb")))
  expect_equal(arrange_cols(dt, at=start, vs), data.table::setcolorder(copy(dt), c("vs", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb")))
  expect_equal(arrange_cols(dt, at="start", "vs"), data.table::setcolorder(copy(dt), c("vs", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb")))
  expect_equal(arrange_cols(dt, at="start", vs, am), data.table::setcolorder(copy(dt), c("vs", "am", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb")))
  expect_equal(arrange_cols(dt, at="end", "vs"), data.table::setcolorder(copy(dt), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb", "vs")))
  expect_equal(arrange_cols(dt, at=end, "vs"), data.table::setcolorder(copy(dt), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "am", "gear", "carb", "vs")))
  expect_equal(arrange_cols(dt, at="start", c("vs", "am")), data.table::setcolorder(copy(dt), c("vs", "am", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb")))
  expect_equal(arrange_cols(dt1, at="vehicle model", vs, am), data.table::setcolorder(copy(dt1), c("vehicle model", "vs", "am", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb")))
  expect_equal(arrange_cols(dt1, at=`vehicle model`, vs, am), data.table::setcolorder(copy(dt1), c("vehicle model", "vs", "am", "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb")))
  expect_equal(arrange_cols(dt1, at="end", `vehicle model`, vs, am), data.table::setcolorder(copy(dt1), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "carb", "vehicle model", "vs", "am")))
  expect_equal(arrange_cols(dt1, at=gear, `vehicle model`, vs, am), data.table::setcolorder(copy(dt1), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "vehicle model", "vs", "am", "carb")))
  expect_equal(arrange_cols(dt1, at=gear, c("vehicle model", "vs", "am")), data.table::setcolorder(copy(dt1), c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "gear", "vehicle model", "vs", "am", "carb")))
})
rm(dt)
rm(dt1)



