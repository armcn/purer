test_that("Lgl_ creates a Lgl_ type", {

  lgl_ <- Lgl_(TRUE)

  expect_true(
    lgl_ %isa% Lgl_
  )

})

test_that("Lgl_ throws if input is not a logical", {

  expect_error(
    Lgl_(1)
  )

})

test_that("Lgl_ throws if input is NA", {

  expect_error(
    Lgl_(NA)
  )

})

test_that("Lgl_ throws if input is an empty vector", {

  expect_error(
    Lgl_(logical(0))
  )

})

test_that("Lgl_ throws if input has attributes", {

  lgl_attr <- c(a = TRUE)

  expect_error(
    Lgl_(lgl_attr)
  )

})
