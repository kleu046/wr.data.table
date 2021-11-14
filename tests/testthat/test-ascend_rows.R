mkdt()
dt1 <- data1 |> dt_keeprownames() |> rn_cols(rn=`vehicle model`)

test_that("ascend_rows without grouping is working", {
  expect_equal(ascend_rows(dt, vs), copy(dt)[order(vs),])
  expect_equal(ascend_rows(dt, "vs"), copy(dt)[order(vs),])
  expect_equal(ascend_rows(dt, vs, am), copy(dt)[order(vs, am),])
  expect_equal(ascend_rows(dt1, c("vehicle model")), copy(dt1)[order(`vehicle model`),])
  expect_equal(ascend_rows(dt1, vs, `vehicle model`), copy(dt1)[order(vs, `vehicle model`),])
})

test_that("ascend_rows with grouping is working", {
  expect_equal(ascend_rows(dt |> set_group(am), vs), copy(dt)[,.SD[order(vs),],by=c("am")])
  expect_equal(ascend_rows(dt |> set_group(am), "vs"), copy(dt)[,.SD[order(vs),],by=c("am")])
  expect_equal(ascend_rows(dt |> set_group(gear), vs, am), copy(dt)[,.SD[order(vs, am),],by=c("gear")])
  expect_equal(ascend_rows(dt1 |> set_group(am), c("vs","vehicle model")), copy(dt1)[,.SD[order(vs, `vehicle model`),],by=c("am")])
  expect_equal(ascend_rows(dt1 |> set_group(am), vs, `vehicle model`), copy(dt1)[,.SD[order(vs, `vehicle model`),],by=c("am")])
})


rm(dt)
rm(dt1)

