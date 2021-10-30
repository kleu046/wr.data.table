test_that("data.table is created with row names", {
  expect_s3_class(dt_keeprownames(data1), "data.table")
  expect_identical(dt_keeprownames(data1),
                   data.table::data.table(rn=rownames(data1),data1))
})
