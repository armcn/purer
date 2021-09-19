test_that("Int_ creates an Int_ type", {

  int_ <- Int_(1L)

  expect_true(
    int_ %isa% Int_
  )

})

test_that("Int_ throws if input is not an integer", {

  expect_error(
    Int_("1")
  )

})

test_that("Int_ throws if input is a double that is not a whole number", {

  expect_error(
    Int_(1.1)
  )

})


test_that("Int_ throws if input is an empty vector", {

  expect_error(
    Int_(integer(0))
  )

})

test_that("Int_ throws if input is NA", {

  expect_error(
    Int_(NA_integer_)
  )

})

test_that("Int_ throws if input has attributes", {

  int_attr <- c(a = 1L)

  expect_error(
    Int_(int_attr)
  )

})
