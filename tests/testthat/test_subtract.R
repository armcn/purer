#-- SCALAR SCALAR

test_operator_S_S(`%-ss%`)

test_that("%-ss%% subtracts numbers", {

  expect_true(
    (Int(1L) %-ss% Int(1L)) ==
      (1L - 1L)
  )

  expect_true(
    (Dbl(1) %-ss% Dbl(1.5)) ==
      (1 - 1.5)
  )

})

test_that("%-ss% return value is a scalar", {

  x <- Int(1L)

  expect_true(
    (x %-ss% x) %isa% S
  )

})

test_that("%-ss% returns the same type as inputs", {

  int <- Int(1L)
  dbl <- Dbl(1)

  expect_true(
    (int %-ss% int) %isa% Int
  )

  expect_true(
    (dbl %-ss% dbl) %isa% Dbl
  )

})

test_that("%-ss% throws if integers and doubles are subtracted", {

  expect_error(
    Int(1L) %-ss% Dbl(1)
  )

})

test_that("%-ss% throws if inputs are not numbers", {

  expect_error(
    Lgl(TRUE) %-ss% Lgl(TRUE)
  )

  expect_error(
    Chr("1") %-ss% Chr("1")
  )

  expect_error(
    Int(1L) %-ss% Lgl(TRUE)
  )

  expect_error(
    Int(1L) %-ss% Chr("1")
  )

})

test_that("%-ss% throws if inputs are base types", {

  expect_error(
    1L %-ss% 1L
  )

  expect_error(
    1 %-ss% 1
  )

})

#-- VECTOR VECTOR

test_operator_V_V(`%-vv%`)

test_that("%-vv% subtracts numbers", {

  expect_true(
    (Int_(1L) %-vv% Int_(1L)) ==
      (1L - 1L)
  )

  expect_true(
    (Dbl_(1) %-vv% Dbl_(1.5)) ==
      (1 - 1.5)
  )

})

test_that("%-vv% return value is a vector", {

  x <- Int_(1L)

  expect_true(
    (x %-vv% x) %isa% V
  )

})

test_that("%-vv% returns the same type as inputs", {

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)

  expect_true(
    (int_ %-vv% int_) %isa% Int_
  )

  expect_true(
    (dbl_ %-vv% dbl_) %isa% Dbl_
  )

})

test_that("%-vv% return value is same length as inputs", {

  int_a <- Int_(1L)
  int_b <- Int_(c(1L, 2L))

  expect_equal(
    length(int_a %-vv% int_a),
    length(int_a)
  )

  expect_equal(
    length(int_b %-vv% int_b),
    length(int_b)
  )

})

test_that("%-vv% throws if inputs are different lengths", {

  expect_error(
    Int_(1L) %-vv% Int_(c(1L, 2L))
  )

})

test_that("%-vv% throws if integers and doubles are subtracted", {

  expect_error(
    Int_(1L) %-vv% Dbl_(1)
  )

})

test_that("%-vv% throws if inputs are not numbers", {

  expect_error(
    Lgl_(TRUE) %-vv% Lgl_(TRUE)
  )

  expect_error(
    Chr_("1") %-vv% Chr_("1")
  )

  expect_error(
    Int_(1L) %-vv% Lgl_(TRUE)
  )

  expect_error(
    Int_(1L) %-vv% Chr_("1")
  )

})

test_that("%-vv% throws if inputs are base types", {

  expect_error(
    1L %-vv% 1L
  )

  expect_error(
    1 %-vv% 1
  )

})

#-- SCALAR VECTOR

test_operator_S_V(`%-sv%`)

test_that("%-sv% subtracts numbers", {

  expect_true(
    (Int(1L) %-sv% Int_(1L)) ==
      (1L - 1L)
  )

  expect_true(
    (Dbl(1) %-sv% Dbl_(1)) ==
      (1 - 1)
  )

})

test_that("%-sv% return value is a vector", {

  int <- Int(1L)
  int_ <- Int_(1L)

  expect_true(
    (int %-sv% int_) %isa% V
  )

})

test_that("%-sv% returns the same type as vector", {

  int <- Int(1L)
  dbl <- Dbl(1)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)

  expect_true(
    (int %-sv% int_) %isa% Int_
  )

  expect_true(
    (dbl %-sv% dbl_) %isa% Dbl_
  )

})

test_that("%-sv% return value is same length as vector", {

  int <- Int(1L)

  int_a <- Int_(1L)
  int_b <- Int_(c(1L, 2L))

  expect_equal(
    length(int_a),
    length(int %-sv% int_a)
  )

  expect_equal(
    length(int_b),
    length(int %-sv% int_b)
  )

})

test_that("%-sv% throws if integers and doubles are subtracted", {

  expect_error(
    Int(1L) %-sv% Dbl_(1)
  )

})

test_that("%-sv% throws if inputs are not numbers", {

  expect_error(
    Lgl(TRUE) %-sv% Lgl_(TRUE)
  )

  expect_error(
    Chr("1") %-sv% Chr_("1")
  )

  expect_error(
    Int(1L) %-sv% Lgl_(TRUE)
  )

  expect_error(
    Int(1L) %-sv% Chr_("1")
  )

})

test_that("%-sv% throws if inputs are base types", {

  expect_error(
    1L %-sv% 1L
  )

  expect_error(
    1 %-sv% 1
  )

})

#-- VECTOR SCALAR

test_operator_V_S(`%-vs%`)

test_that("%-vs% subtracts numbers", {

  expect_true(
    (Int_(1L) %-vs% Int(1L)) ==
      (1L - 1L)
  )

  expect_true(
    (Dbl_(1) %-vs% Dbl(1)) ==
      (1 - 1)
  )

})

test_that("%-vs% return value is a vector", {

  int <- Int(1L)
  int_ <- Int_(1L)

  expect_true(
    (int_ %-vs% int) %isa% V
  )

})

test_that("%-vs% returns the same type as vector", {

  int <- Int(1L)
  dbl <- Dbl(1)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)

  expect_true(
    (int_ %-vs% int) %isa% Int_
  )

  expect_true(
    (dbl_ %-vs% dbl) %isa% Dbl_
  )

})

test_that("%-vs% return value is same length as vector", {

  int <- Int(1L)

  int_a <- Int_(1L)
  int_b <- Int_(c(1L, 2L))

  expect_equal(
    length(int_a),
    length(int_a %-vs% int)
  )

  expect_equal(
    length(int_b),
    length(int_b %-vs% int)
  )

})

test_that("%-vs% throws if integers and doubles are subtracted", {

  expect_error(
    Int_(1L) %-vs% Dbl(1)
  )

})

test_that("%-vs% throws if inputs are not numbers", {

  expect_error(
    Lgl_(TRUE) %-vs% Lgl(TRUE)
  )

  expect_error(
    Chr_("1") %-vs% Chr("1")
  )

  expect_error(
    Int_(1L) %-vs% Lgl(TRUE)
  )

  expect_error(
    Int_(1L) %-vs% Chr("1")
  )

})

test_that("%-vs% throws if inputs are base types", {

  expect_error(
    1L %-vs% 1L
  )

  expect_error(
    1 %-vs% 1
  )

})
