test_that("data.table is created with row names", {
  expect_s3_class(wr_dt_withrownames(data1), "data.table")
  expect_identical(wr_dt_withrownames(data1),
                   data.table::data.table(rn=rownames(data1),data1))
})
