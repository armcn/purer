test_that("Dbl_ creates a Dbl_ type", {

  dbl_ <- Dbl_(1)

  expect_true(
    dbl_ %isa% Dbl_
  )

})

test_that("Dbl_ throws if input is not a double", {

  expect_error(
    Dbl_(1L)
  )

})

test_that("Dbl_ throws if input is an empty vector", {

  expect_error(
    Dbl_(double(0))
  )

})

test_that("Dbl_ throws if input is NA", {

  expect_error(
    Dbl_(NA_real_)
  )

})

test_that("Dbl_ throws if input is NaN", {

  expect_error(
    Dbl_(NaN)
  )

})

test_that("Dbl_ throws if input is infinite", {

  expect_error(
    Dbl(Inf)
  )

  expect_error(
    Dbl(-Inf)
  )

})

test_that("Dbl_ throws if input has attributes", {

  dbl_attr <- c(a = 1)

  expect_error(
    Dbl_(dbl_attr)
  )

})
