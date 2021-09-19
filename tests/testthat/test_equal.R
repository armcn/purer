#-- pure_equal

test_that("pure_equal throws if with invalid value of .types", {

  int <- Int(1L)

  expect_error(
    pure_equal(.x = int, .y = int, .types = Chr("SS"))
  )

  expect_error(
    pure_equal(.x = int, .y = int, .types = Chr("s"))
  )

  expect_error(
    pure_equal(.x = int, .y = int, .types = Chr("se"))
  )

})

test_that("pure_equal returns correct values", {

  expect_equal(
    pure_equal(
      .x = Int(1L),
      .y = Int(1L),
      .types = Chr("ss")
    ),
    Lgl(TRUE)
  )

  expect_equal(
    pure_equal(
      .x = Int(1L),
      .y = Int_(c(1L, 2L)),
      .types = Chr("sv")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Int_(c(1L, 2L)),
      .y = Int(1L),
      .types = Chr("vs")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Int_(c(1L, 2L)),
      .y = Int_(c(1L, 2L)),
      .types = Chr("vv")
    ),
    Lgl_(c(TRUE, TRUE))
  )

  expect_equal(
    pure_equal(
      .x = Dbl(1),
      .y = Dbl(1),
      .types = Chr("ss")
    ),
    Lgl(TRUE)
  )

  expect_equal(
    pure_equal(
      .x = Dbl(1),
      .y = Dbl_(c(1, 2)),
      .types = Chr("sv")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Dbl_(c(1, 2)),
      .y = Dbl(1),
      .types = Chr("vs")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Dbl_(c(1, 2)),
      .y = Dbl_(c(1, 2)),
      .types = Chr("vv")
    ),
    Lgl_(c(TRUE, TRUE))
  )

  expect_equal(
    pure_equal(
      .x = Chr("a"),
      .y = Chr("a"),
      .types = Chr("ss")
    ),
    Lgl(TRUE)
  )

  expect_equal(
    pure_equal(
      .x = Chr("a"),
      .y = Chr_(c("a", "b")),
      .types = Chr("sv")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Chr_(c("a", "b")),
      .y = Chr("a"),
      .types = Chr("vs")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Chr_(c("a", "b")),
      .y = Chr_(c("a", "b")),
      .types = Chr("vv")
    ),
    Lgl_(c(TRUE, TRUE))
  )

  expect_equal(
    pure_equal(
      .x = Lgl(TRUE),
      .y = Lgl(TRUE),
      .types = Chr("ss")
    ),
    Lgl(TRUE)
  )

  expect_equal(
    pure_equal(
      .x = Lgl(TRUE),
      .y = Lgl_(c(TRUE, FALSE)),
      .types = Chr("sv")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Lgl_(c(TRUE, FALSE)),
      .y = Lgl(TRUE),
      .types = Chr("vs")
    ),
    Lgl_(c(TRUE, FALSE))
  )

  expect_equal(
    pure_equal(
      .x = Lgl_(c(TRUE, FALSE)),
      .y = Lgl_(c(TRUE, FALSE)),
      .types = Chr("vv")
    ),
    Lgl_(c(TRUE, TRUE))
  )

})

#-- SCALAR SCALAR

test_operator_S_S(.f = `%==ss%`)

test_that("%==ss% return value is Lgl", {

  x <- Int(1L)

  expect_true(
    (x %==ss% x) %isa% Lgl
  )

})

test_that("%==ss% returns TRUE for all scalar types", {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  expect_true(
    int %==ss% int
  )

  expect_true(
    dbl %==ss% dbl
  )

  expect_true(
    chr %==ss% chr
  )

  expect_true(
    lgl %==ss% lgl
  )

})

test_that("%==ss% returns FALSE for non-equal scalars", {

  expect_false(
    Int(1L) %==ss% Int(2L)
  )

  expect_false(
    Dbl(1) %==ss% Dbl(2)
  )

  expect_false(
    Chr("a") %==ss% Chr("b")
  )

  expect_false(
    Lgl(TRUE) %==ss% Lgl(FALSE)
  )

})

test_that("%==ss% throws if mismatched types are compared", {

  expect_error(
    Int(1L) %==ss% Dbl(1)
  )

  expect_error(
    Int(1L) %==ss% Lgl(TRUE)
  )

  expect_error(
    Int(1L) %==ss% Chr("1")
  )

})

test_that("%==ss% throws if inputs are base types", {

  expect_error(
    1L %==ss% 1L
  )

  expect_error(
    1 %==ss% 1
  )

  expect_error(
    "a" %==ss% "a"
  )

  expect_error(
    TRUE %==ss% TRUE
  )

})

#-- VECTOR VECTOR

test_operator_V_V(.f = `%==vv%`)

test_that("%==vv% return value is equal to length of inputs", {

  x <- Int_(c(1L, 2L))

  return_val <- x %==vv% x

  expect_equal(
    length(x),
    length(return_val)
  )

})

test_that("%==vv% throws if inputs are different lengths", {

  x <- Int_(c(1L))
  y <- Int_(c(1L, 2L))

  expect_error(
    x %==vv% y
  )

})

test_that("%==vv% return value is Lgl_", {

  x <- Int_(1L)

  expect_true(
    (x %==vv% x) %isa% Lgl_
  )

})

test_that("%==vv% returns TRUE for all vector types", {

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  expect_true(
    int_ %==vv% int_
  )

  expect_true(
    dbl_ %==vv% dbl_
  )

  expect_true(
    chr_ %==vv% chr_
  )

  expect_true(
    lgl_ %==vv% lgl_
  )

})

test_that("%==vv% returns FALSE for non-equal vectors", {

  expect_false(
    Int_(1L) %==vv% Int_(2L)
  )

  expect_false(
    Dbl_(1) %==vv% Dbl_(2)
  )

  expect_false(
    Chr_("a") %==vv% Chr_("b")
  )

  expect_false(
    Lgl_(TRUE) %==vv% Lgl_(FALSE)
  )

})

test_that("%==vv% throws if mismatched types are compared", {

  expect_error(
    Int_(1L) %==vv% Dbl_(1)
  )

  expect_error(
    Int_(1L) %==vv% Lgl_(TRUE)
  )

  expect_error(
    Int_(1L) %==vv% Chr_("1")
  )

})

test_that("%==vv% throws if inputs are base types", {

  expect_error(
    1L %==vv% 1L
  )

  expect_error(
    1 %==vv% 1
  )

  expect_error(
    "a" %==vv% "a"
  )

  expect_error(
    TRUE %==vv% TRUE
  )

})

#-- SCALAR VECTOR

test_operator_S_V(`%==sv%`)

test_that("%==sv% return value is equal to length of vector", {

  scalar <- Int(1L)
  vector_a <- Int_(1L)
  vector_b <- Int_(c(1L, 2L))
  vector_c <- Int_(c(1L, 2L, 3L))

  expect_equal(
    length(vector_a),
    length(scalar %==sv% vector_a)
  )

  expect_equal(
    length(vector_b),
    length(scalar %==sv% vector_b)
  )

  expect_equal(
    length(vector_c),
    length(scalar %==sv% vector_c)
  )

})

test_that("%==sv% return value is Lgl_", {

  x <- Int(1L)
  y <- Int_(1L)

  expect_true(
    (x %==sv% y) %isa% Lgl_
  )

})

test_that("%==sv% returns TRUE for all vector types", {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  expect_true(
    int %==sv% int_
  )

  expect_true(
    dbl %==sv% dbl_
  )

  expect_true(
    chr %==sv% chr_
  )

  expect_true(
    lgl %==sv% lgl_
  )

})

test_that("%==sv% returns FALSE for non-equal inputs", {

  expect_false(
    Int(1L) %==sv% Int_(2L)
  )

  expect_false(
    Dbl(1) %==sv% Dbl_(2)
  )

  expect_false(
    Chr("a") %==sv% Chr_("b")
  )

  expect_false(
    Lgl(TRUE) %==sv% Lgl_(FALSE)
  )

})

test_that("%==sv% throws if mismatched types are compared", {

  expect_error(
    Int(1L) %==sv% Dbl_(1)
  )

  expect_error(
    Int(1L) %==sv% Lgl_(TRUE)
  )

  expect_error(
    Int(1L) %==sv% Chr_("1")
  )

})

test_that("%==sv% throws if inputs are base types", {

  expect_error(
    1L %==sv% 1L
  )

  expect_error(
    1 %==sv% 1
  )

  expect_error(
    "a" %==sv% "a"
  )

  expect_error(
    TRUE %==sv% TRUE
  )

})

#-- VECTOR SCALAR

test_operator_V_S(`%==vs%`)

test_that("%==vs% return value is equal to length of vector", {

  scalar <- Int(1L)
  vector_a <- Int_(1L)
  vector_b <- Int_(c(1L, 2L))
  vector_c <- Int_(c(1L, 2L, 3L))

  expect_equal(
    length(vector_a),
    length(vector_a %==vs% scalar)
  )

  expect_equal(
    length(vector_b),
    length(vector_b %==vs% scalar)
  )

  expect_equal(
    length(vector_c),
    length(vector_c %==vs% scalar)
  )

})

test_that("%==vs% return value is Lgl_", {

  x <- Int_(1L)
  y <- Int(1L)

  expect_true(
    (x %==vs% y) %isa% Lgl_
  )

})

test_that("%==vs% returns TRUE for all vector types", {

  int <- Int(1L)
  dbl <- Dbl(1)
  chr <- Chr("a")
  lgl <- Lgl(TRUE)

  int_ <- Int_(1L)
  dbl_ <- Dbl_(1)
  chr_ <- Chr_("a")
  lgl_ <- Lgl_(TRUE)

  expect_true(
    int_ %==vs% int
  )

  expect_true(
    dbl_ %==vs% dbl
  )

  expect_true(
    chr_ %==vs% chr
  )

  expect_true(
    lgl_ %==vs% lgl
  )

})

test_that("%==vs% returns FALSE for non-equal inputs", {

  expect_false(
    Int_(1L) %==vs% Int(2L)
  )

  expect_false(
    Dbl_(1) %==vs% Dbl(2)
  )

  expect_false(
    Chr_("a") %==vs% Chr("b")
  )

  expect_false(
    Lgl_(TRUE) %==vs% Lgl(FALSE)
  )

})

test_that("%==vs% throws if mismatched types are compared", {

  expect_error(
    Int_(1L) %==vs% Dbl(1)
  )

  expect_error(
    Int_(1L) %==vs% Lgl(TRUE)
  )

  expect_error(
    Int_(1L) %==vs% Chr("1")
  )

})

test_that("%==vs% throws if inputs are base types", {

  expect_error(
    1L %==vs% 1L
  )

  expect_error(
    1 %==vs% 1
  )

  expect_error(
    "a" %==vs% "a"
  )

  expect_error(
    TRUE %==vs% TRUE
  )

})
