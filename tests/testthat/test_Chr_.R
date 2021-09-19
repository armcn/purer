test_that("Chr_ creates a Chr_ type", {

  chr_ <- Chr_("a")

  expect_true(
    chr_ %isa% Chr_
  )

})

test_that("Chr_ throws if input is not a character", {

  expect_error(
    Chr_(1)
  )

})

test_that("Chr_ throws if input is an NA_character", {

  expect_error(
    Chr_(NA_character_)
  )

})

test_that("Chr_ throws if input is an empty vector", {

  expect_error(
    Chr_(character(0))
  )

})

test_that("Chr_ throws if input has attributes", {

  chr_attr <- c(a = "b")

  expect_error(
    Chr_(chr_attr)
  )

})
