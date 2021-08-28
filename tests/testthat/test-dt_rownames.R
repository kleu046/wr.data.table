test_that("data.table is created with row names", {
  expect_that(wr_dt_withrownames(mtcars), is_a("data.table"))
  expect_identical(wr_dt_withrownames(mtcars),
                   data.table(rn=rownames(mtcars),mtcars))
})
