mkdt()
test_that("spread is working", {
  expect_identical(dcast(copy(dt),cyl + disp + hp + drat + wt + qsec + vs + am + carb ~ gear,value.var="mpg"),
                   spread(copy(dt),gear, mpg))
})
rm(dt)
