test_that("Chr creates a Chr type", {

  chr <- Chr("a")

  expect_true(
    chr %isa% Chr
  )

})

test_that("Chr throws if input is not a character", {

  expect_error(
    Chr(1)
  )

})

test_that("Chr throws if input is an NA_character", {

  expect_error(
    Chr(NA_character_)
  )

})

test_that("Chr throws if input is an empty vector", {

  expect_error(
    Chr(character(0))
  )

})

test_that("Chr throws if input has length > 1", {

  vector_chr <- c("a", "b")

  expect_error(
    Chr(vector_chr)
  )

})

test_that("Chr throws if input has attributes", {

  chr_attr <- c(a = "b")

  expect_error(
    Chr(chr_attr)
  )

})
