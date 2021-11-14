mkdt()
dt1 <- data1 |> dt_keeprownames() |> rn_cols(rn=`vehicle model`)

test_that("descend_rows without grouping is working", {
  expect_equal(descend_rows(dt, vs), copy(dt)[order(vs, decreasing=T),])
  expect_equal(descend_rows(dt, "vs"), copy(dt)[order(vs, decreasing=T),])
  expect_equal(descend_rows(dt, vs, am), copy(dt)[order(vs, am, decreasing=T),])
  expect_equal(descend_rows(dt1, c("vehicle model")), copy(dt1)[order(`vehicle model`, decreasing=T),])
  expect_equal(descend_rows(dt1, vs, `vehicle model`), copy(dt1)[order(vs, `vehicle model`, decreasing=T),])
})

test_that("descend_rows with grouping is working", {
  expect_equal(descend_rows(dt |> set_group(am), vs), copy(dt)[,.SD[order(vs, decreasing=T),],by=c("am")])
  expect_equal(descend_rows(dt |> set_group(am), "vs"), copy(dt)[,.SD[order(vs, decreasing=T),],by=c("am")])
  expect_equal(descend_rows(dt |> set_group(gear), vs, am), copy(dt)[,.SD[order(vs, am, decreasing=T),],by=c("gear")])
  expect_equal(descend_rows(dt1 |> set_group(am), c("vs","vehicle model")), copy(dt1)[,.SD[order(vs, `vehicle model`, decreasing=T),],by=c("am")])
  expect_equal(descend_rows(dt1 |> set_group(am), vs, `vehicle model`), copy(dt1)[,.SD[order(vs, `vehicle model`, decreasing=T),],by=c("am")])
})


rm(dt)
rm(dt1)

