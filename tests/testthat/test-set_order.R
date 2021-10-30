mkdt()
dt_ordered <- set_order(dt, vs, am)

test_that("data.table is ordered", {
  expect_s3_class(dt_ordered, "data.table")
  expect_identical(dt_ordered, dt[order(vs, am),])
})

rm(dt)
