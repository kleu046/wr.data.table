mkdt()
test_that("rn_cols is working", {
  expect_s3_class(rn_cols(dt, mpg = `miles per gallon`), "data.table")

  expect_identical({
    mkdt()
    rn_cols(dt, mpg = `miles per gallon`)
  },
  {
    mkdt()
    rn_cols(dt, "mpg", "miles per gallon")
  })

  expect_identical({
    mkdt()
    rn_cols(dt, mpg = `miles per gallon`, hp = hp2)
  },
  {
    mkdt()
    rn_cols(dt, c("mpg", "hp"), c("miles per gallon", "hp2"))
  })

  expect_identical({
      mkdt()
      rn_cols(dt, mpg = `miles per gallon`, disp = displacement)
    },
    {
      mkdt()
      rn_cols(dt, c("mpg", "disp"), c("miles per gallon", "displacement"))
    })

  expect_identical({
    mkdt()
    rn_cols(dt, mpg = `miles per gallon`)
  },
  {
    mkdt()
    setnames(dt, "mpg", "miles per gallon")
  })

  expect_identical({
    mkdt()
    rn_cols(dt, mpg = `mpg1`)
  },
  {
    mkdt()
    setnames(dt, "mpg", "mpg1")
  })

  expect_identical({
    mkdt()
    rn_cols(dt, `mpg` = `m p g`)
  },
  {
    mkdt()
    setnames(copy(dt), "mpg", "m p g")
  })

  expect_identical({
    mkdt()
    rn_cols(dt, mpg = `m p g`)
  },
  {
    mkdt()
    setnames(dt, "mpg", "m p g")
  })

  mkdt()
  dt1 <- rn_cols(copy(dt), mpg = `miles per gallon`)
  expect_true(!identical(dt1, dt))
  rm(dt1)
})
rm(dt)


