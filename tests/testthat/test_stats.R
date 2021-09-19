#-- pure_min

test_that("pure_min returns min value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_min() |> drop_pure_type(),
    min(x)
  )

})

test_that("pure_min throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_min(base_int)
  )

  expect_error(
    pure_min(base_dbl)
  )

})

test_that("pure_min throws if input isn't an N type", {

  expect_error(
    pure_min(Chr_("a"))
  )

})

test_that("pure_min return value is a scalar", {

  val <- pure_min(Int_(c(1L, 2L)))

  expect_true(
    val %isa% S
  )

})

test_that("pure_min return value is same type as input", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_equal(
    x_int |> attr("type"),
    pure_min(x_int) |> attr("type")
  )

  expect_equal(
    x_dbl |> attr("type"),
    pure_min(x_dbl) |> attr("type")
  )

})

#-- pure_min_

test_that("pure_min_ returns min value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_min_() |> drop_pure_type(),
    min(x)
  )

})

test_that("pure_min_ throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_min_(base_int)
  )

  expect_error(
    pure_min_(base_dbl)
  )

})

test_that("pure_min_ throws if input isn't an N type", {

  expect_error(
    pure_min_(Chr_("a"))
  )

})

test_that("pure_min_ return value is a vector", {

  val <- pure_min_(Int_(c(1L, 2L)))

  expect_true(
    val %isa% V
  )

})

test_that("pure_min_ return value is same type as input", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_equal(
    x_int |> attr("type"),
    pure_min_(x_int) |> attr("type")
  )

  expect_equal(
    x_dbl |> attr("type"),
    pure_min_(x_dbl) |> attr("type")
  )

})

#-- pure_max

test_that("pure_max returns max value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_max() |> drop_pure_type(),
    max(x)
  )

})

test_that("pure_max throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_max(base_int)
  )

  expect_error(
    pure_max(base_dbl)
  )

})

test_that("pure_max throws if input isn't an N type", {

  expect_error(
    pure_max(Chr_("a"))
  )

})

test_that("pure_max return value is a scalar", {

  val <- pure_max(Int_(c(1L, 2L)))

  expect_true(
    val %isa% S
  )

})

test_that("pure_max return value is same type as input", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_equal(
    x_int |> attr("type"),
    pure_max(x_int) |> attr("type")
  )

  expect_equal(
    x_dbl |> attr("type"),
    pure_max(x_dbl) |> attr("type")
  )

})

#-- pure_max_

test_that("pure_max_ returns max value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_max_() |> drop_pure_type(),
    max(x)
  )

})

test_that("pure_max_ throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_max_(base_int)
  )

  expect_error(
    pure_max_(base_dbl)
  )

})

test_that("pure_max_ throws if input isn't an N type", {

  expect_error(
    pure_max_(Chr_("a"))
  )

})

test_that("pure_max_ return value is a vector", {

  val <- pure_max_(Int_(c(1L, 2L)))

  expect_true(
    val %isa% V
  )

})

test_that("pure_max_ return value is same type as input", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_equal(
    x_int |> attr("type"),
    pure_max_(x_int) |> attr("type")
  )

  expect_equal(
    x_dbl |> attr("type"),
    pure_max_(x_dbl) |> attr("type")
  )

})

#-- pure_mean

test_that("pure_mean returns mean value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_mean() |> drop_pure_type(),
    mean(x)
  )

})

test_that("pure_mean throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_mean(base_int)
  )

  expect_error(
    pure_mean(base_dbl)
  )

})

test_that("pure_mean throws if input isn't an N type", {

  expect_error(
    pure_mean(Chr_("a"))
  )

})

test_that("pure_mean return value is a scalar", {

  val <- pure_mean(Int_(c(1L, 2L)))

  expect_true(
    val %isa% S
  )

})

test_that("pure_mean return value is a Dbl", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_true(
    pure_mean(x_int) %isa% Dbl
  )

  expect_true(
    pure_mean(x_dbl) %isa% Dbl
  )

})

#-- pure_mean_

test_that("pure_mean_ returns mean value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_mean_() |> drop_pure_type(),
    mean(x)
  )

})

test_that("pure_mean_ throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_mean_(base_int)
  )

  expect_error(
    pure_mean_(base_dbl)
  )

})

test_that("pure_mean_ throws if input isn't an N type", {

  expect_error(
    pure_mean_(Chr_("a"))
  )

})

test_that("pure_mean_ return value is a vector", {

  val <- pure_mean_(Int_(c(1L, 2L)))

  expect_true(
    val %isa% V
  )

})

test_that("pure_mean_ return value is a Dbl_", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_true(
    pure_mean_(x_int) %isa% Dbl_
  )

  expect_true(
    pure_mean_(x_dbl) %isa% Dbl_
  )

})

#-- pure_median

test_that("pure_median returns median value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_median() |> drop_pure_type(),
    median(x)
  )

})

test_that("pure_median throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_median(base_int)
  )

  expect_error(
    pure_median(base_dbl)
  )

})

test_that("pure_median throws if input isn't an N type", {

  expect_error(
    pure_median(Chr_("a"))
  )

})

test_that("pure_median return value is a scalar", {

  val <- pure_median(Int_(c(1L, 2L)))

  expect_true(
    val %isa% S
  )

})

test_that("pure_median return value is a Dbl", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_true(
    pure_median(x_int) %isa% Dbl
  )

  expect_true(
    pure_median(x_dbl) %isa% Dbl
  )

})

#-- pure_median_

test_that("pure_median_ returns median value of vector", {

  x <- c(1L, 2L)

  expect_equal(
    x |> Int_() |> pure_median_() |> drop_pure_type(),
    median(x)
  )

})

test_that("pure_median_ throws if input isn't a pure vector", {

  base_int <- c(1L, 2L)
  base_dbl <- as.double(c(1, 2))

  expect_error(
    pure_median_(base_int)
  )

  expect_error(
    pure_median_(base_dbl)
  )

})

test_that("pure_median_ throws if input isn't an N type", {

  expect_error(
    pure_median_(Chr_("a"))
  )

})

test_that("pure_median_ return value is a vector", {

  val <- pure_median_(Int_(c(1L, 2L)))

  expect_true(
    val %isa% V
  )

})

test_that("pure_median_ return value is a Dbl_", {

  x_int <- Int_(c(1L, 2L))
  x_dbl <- Dbl_(as.double(c(1, 2)))

  expect_true(
    pure_median_(x_int) %isa% Dbl_
  )

  expect_true(
    pure_median_(x_dbl) %isa% Dbl_
  )

})

