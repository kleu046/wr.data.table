mkdt()
myvar <- "mpg"
test_that("condense without grouping is working", {
  expect_equal(condense(dt, mu_mpg = mean(mpg)), copy(dt)[,.(mu_mpg = mean(mpg))])
  expect_equal(condense(dt, mu_mpg = mean("mpg")), copy(dt)[,.(mu_mpg = mean(mpg))])
  expect_equal(condense(dt, mu_mpg = mean(mpg), sd_mpg = sd(mpg)), copy(dt)[,.(mu_mpg = mean(mpg), sd_mpg = sd(mpg))])
})

test_that("condense with grouping is working", {
  expect_equal(condense(dt |> set_group(vs), mu_mpg = mean(mpg)), copy(dt)[,.(mu_mpg = mean(mpg)), by=c("vs")])
  expect_equal(condense(dt |> set_group(vs), mu_mpg = mean(eval(char_to_symbol(myvar)))), copy(dt)[,.(mu_mpg = mean(mpg)), by=c("vs")])
  expect_equal(condense(dt |> set_group(vs), mu_mpg = mean("mpg")), copy(dt)[,.(mu_mpg = mean(mpg)), by=c("vs")])
  expect_equal(condense(dt |> set_group(vs), mu_mpg = mean("mpg"), sd_mpg = sd(mpg)), copy(dt)[,.(mu_mpg = mean(mpg), sd_mpg = sd(mpg)), by=c("vs")])
})

rm(dt)
