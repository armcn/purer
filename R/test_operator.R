test_operator_S_S(.f) %when% {

  is.function(.f)

} %as% {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  test_that("operator throws if lhs is a vector", {

    expect_error(
      .f(int_, int)
    )

    expect_error(
      .f(dbl_, dbl)
    )

    expect_error(
      .f(chr_, chr)
    )

    expect_error(
      .f(lgl_, lgl)
    )

  })

  test_that("operator throws if rhs is a vector", {

    expect_error(
      .f(int, int_)
    )

    expect_error(
      .f(dbl, dbl_)
    )

    expect_error(
      .f(chr, chr_)
    )

    expect_error(
      .f(lgl, lgl_)
    )

  })

  test_that("operator throws if lhs and rhs are vectors", {

    expect_error(
      .f(int_, int_)
    )

    expect_error(
      .f(dbl_, dbl_)
    )

    expect_error(
      .f(chr_, chr_)
    )

    expect_error(
      .f(lgl_, lgl_)
    )

  })

}

test_operator_V_V(.f) %when% {

  is.function(.f)

} %as% {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  test_that("operator throws if lhs is a scalar", {

    expect_error(
      .f(int, int_)
    )

    expect_error(
      .f(dbl, dbl_)
    )

    expect_error(
      .f(chr, chr_)
    )

    expect_error(
      .f(lgl, lgl_)
    )

  })

  test_that("operator throws if rhs is a scalar", {

    expect_error(
      .f(int_, int)
    )

    expect_error(
      .f(dbl_, dbl)
    )

    expect_error(
      .f(chr_, chr)
    )

    expect_error(
      .f(lgl_, lgl)
    )

  })

  test_that("operator throws if lhs and rhs are scalars", {

    expect_error(
      .f(int, int)
    )

    expect_error(
      .f(dbl, dbl)
    )

    expect_error(
      .f(chr, chr)
    )

    expect_error(
      .f(lgl, lgl)
    )

  })

}

test_operator_S_V(.f) %when% {

  is.function(.f)

} %as% {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  test_that("operator throws if lhs is a vector", {

    expect_error(
      .f(int_, int)
    )

    expect_error(
      .f(dbl_, dbl)
    )

    expect_error(
      .f(chr_, chr)
    )

    expect_error(
      .f(lgl_, lgl)
    )

  })

  test_that("operator throws if lhs and rhs are scalars", {

    expect_error(
      .f(int, int)
    )

    expect_error(
      .f(dbl, dbl)
    )

    expect_error(
      .f(chr, chr)
    )

    expect_error(
      .f(lgl, lgl)
    )

  })

  test_that("operator throws if lhs and rhs are vectors", {

    expect_error(
      .f(int_, int_)
    )

    expect_error(
      .f(dbl_, dbl_)
    )

    expect_error(
      .f(chr_, chr_)
    )

    expect_error(
      .f(lgl_, lgl_)
    )

  })

}

test_operator_V_S(.f) %when% {

  is.function(.f)

} %as% {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  test_that("operator throws if rhs is a vector", {

    expect_error(
      .f(int, int_)
    )

    expect_error(
      .f(dbl, dbl_)
    )

    expect_error(
      .f(chr, chr_)
    )

    expect_error(
      .f(lgl, lgl_)
    )

  })

  test_that("operator throws if lhs and rhs are scalars", {

    expect_error(
      .f(int, int)
    )

    expect_error(
      .f(dbl, dbl)
    )

    expect_error(
      .f(chr, chr)
    )

    expect_error(
      .f(lgl, lgl)
    )

  })

  test_that("operator throws if lhs and rhs are vectors", {

    expect_error(
      .f(int_, int_)
    )

    expect_error(
      .f(dbl_, dbl_)
    )

    expect_error(
      .f(chr_, chr_)
    )

    expect_error(
      .f(lgl_, lgl_)
    )

  })

}
