test_that("Lgl creates a Lgl type", {

  lgl <- Lgl(TRUE)

  expect_true(
    lgl %isa% Lgl
  )

})

test_that("Lgl throws if input is not a logical", {

  expect_error(
    Lgl(1)
  )

})

test_that("Lgl throws if input is NA", {

  expect_error(
    Lgl(NA)
  )

})

test_that("Lgl throws if input is an empty vector", {

  expect_error(
    Lgl(logical(0))
  )

})

test_that("Lgl throws if input has length > 1", {

  vector_lgl <- c(TRUE, TRUE)

  expect_error(
    Lgl(vector_lgl)
  )

})

test_that("Lgl throws if input has attributes", {

  lgl_attr <- c(a = TRUE)

  expect_error(
    Lgl(lgl_attr)
  )

})
