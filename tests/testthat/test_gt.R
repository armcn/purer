#-- pure_gt

test_that("pure_gt throws if with invalid value of .types", {

  int <- Int(1L)

  expect_error(
    pure_gt(.x = int, .y = int, .types = Chr("SS"))
  )

  expect_error(
    pure_gt(.x = int, .y = int, .types = Chr("s"))
  )

  expect_error(
    pure_gt(.x = int, .y = int, .types = Chr("se"))
  )

})

test_that("pure_gt returns correct values", {

  expect_equal(
    pure_gt(
      .x = Int(1L),
      .y = Int(1L),
      .types = Chr("ss")
    ),
    Lgl(FALSE)
  )

  expect_equal(
    pure_gt(
      .x = Int(1L),
      .y = Int_(c(1L, 2L)),
      .types = Chr("sv")
    ),
    Lgl_(c(FALSE, FALSE))
  )

  expect_equal(
    pure_gt(
      .x = Int_(c(1L, 2L)),
      .y = Int(1L),
      .types = Chr("vs")
    ),
    Lgl_(c(FALSE, TRUE))
  )

  expect_equal(
    pure_gt(
      .x = Int_(c(1L, 2L)),
      .y = Int_(c(1L, 2L)),
      .types = Chr("vv")
    ),
    Lgl_(c(FALSE, FALSE))
  )

  expect_equal(
    pure_gt(
      .x = Dbl(1),
      .y = Dbl(1),
      .types = Chr("ss")
    ),
    Lgl(FALSE)
  )

  expect_equal(
    pure_gt(
      .x = Dbl(1),
      .y = Dbl_(c(1, 2)),
      .types = Chr("sv")
    ),
    Lgl_(c(FALSE, FALSE))
  )

  expect_equal(
    pure_gt(
      .x = Dbl_(c(1, 2)),
      .y = Dbl(1),
      .types = Chr("vs")
    ),
    Lgl_(c(FALSE, TRUE))
  )

  expect_equal(
    pure_gt(
      .x = Dbl_(c(1, 2)),
      .y = Dbl_(c(1, 2)),
      .types = Chr("vv")
    ),
    Lgl_(c(FALSE, FALSE))
  )

})

#-- SCALAR SCALAR

test_operator_S_S(.f = `%>ss%`)

test_that("%>ss% return value is Lgl", {

  x <- Int(1L)

  expect_true(
    (x %>ss% x) %isa% Lgl
  )

})

test_that("%>ss% returns correct values", {

  expect_false(
    Int(1L) %>ss% Int(1L)
  )

  expect_true(
    Int(2L) %>ss% Int(1L)
  )

  expect_false(
    Dbl(1) %>ss% Dbl(1)
  )

  expect_true(
    Dbl(2) %>ss% Dbl(1)
  )

})

test_that("%>ss% throws if inputs are not numbers", {

  expect_error(
    Lgl(TRUE) %>ss% Lgl(TRUE)
  )

  expect_error(
    Chr("a") %>ss% Chr("b")
  )

})

test_that("%>ss% throws if an Int and Dbl are compared", {

  expect_error(
    Int(1L) %>ss% Dbl(1)
  )

  expect_error(
    Dbl(1) %>ss% Int(1L)
  )

})

test_that("%>ss% throws if inputs are base types", {

  expect_error(
    1 %>ss% 1
  )

  expect_error(
    1L %>ss% 1L
  )

})

#-- VECTOR VECTOR

test_operator_V_V(.f = `%>vv%`)

test_that("%>vv% return value is equal to length of inputs", {

  x <- Int_(c(1L, 2L))

  return_val <- x %>vv% x

  expect_equal(
    length(x),
    length(return_val)
  )

})

test_that("%>vv% throws if inputs are different lengths", {

  x <- Int_(c(1L))
  y <- Int_(c(1L, 2L))

  expect_error(
    x %>vv% y
  )

})

test_that("%>vv% return value is Lgl_", {

  x <- Int_(1L)

  expect_true(
    (x %>vv% x) %isa% Lgl_
  )

})

test_that("%>vv% returns correct values", {

  expect_equal(
    Int_(c(1L, 2L)) %>vv% Int_(c(1L, 1L)),
    Lgl_(c(FALSE, TRUE))
  )

  expect_equal(
    Dbl_(c(1, 2)) %>vv% Dbl_(c(1, 1)),
    Lgl_(c(FALSE, TRUE))
  )

})

test_that("%>vv% throws if inputs are not numbers", {

  expect_error(
    Lgl_(TRUE) %>vv% Lgl_(TRUE)
  )

  expect_error(
    Chr_("a") %>vv% Chr_("b")
  )

})

test_that("%>vv% throws if an Int and Dbl vector are compared", {

  expect_error(
    Int_(c(1L, 1L)) %>vv% Dbl_(c(1, 1))
  )

  expect_error(
    Dbl_(c(1, 1)) %>vv% Int_(c(1L, 1L))
  )

})

test_that("%>vv% throws if inputs are base types", {

  expect_error(
    c(1, 2) %>vv% c(1, 2)
  )

  expect_error(
    c(1L, 2L) %>vv% c(1L, 2L)
  )

})

#-- SCALAR VECTOR

test_operator_S_V(`%>sv%`)

test_that("%>sv% return value is equal to length of vector", {

  scalar <- Int(1L)
  vector_a <- Int_(1L)
  vector_b <- Int_(c(1L, 2L))
  vector_c <- Int_(c(1L, 2L, 3L))

  expect_equal(
    length(vector_a),
    length(scalar %>sv% vector_a)
  )

  expect_equal(
    length(vector_b),
    length(scalar %>sv% vector_b)
  )

  expect_equal(
    length(vector_c),
    length(scalar %>sv% vector_c)
  )

})

test_that("%>sv% return value is Lgl_", {

  x <- Int(1L)
  y <- Int_(1L)

  expect_true(
    (x %>sv% y) %isa% Lgl_
  )

})

test_that("%>sv% returns correct values", {

  expect_equal(
    Int(1L) %>sv% Int_(c(1L, 1L)),
    Lgl_(c(FALSE, FALSE))
  )

  expect_equal(
    Dbl(1) %>sv% Dbl_(c(1, 1)),
    Lgl_(c(FALSE, FALSE))
  )

})

test_that("%>sv% throws if inputs are not numbers", {

  expect_error(
    Lgl(TRUE) %>sv% Lgl_(TRUE)
  )

  expect_error(
    Chr("a") %>sv% Chr_("b")
  )

})

test_that("%>sv% throws if an Int and Dbl are compared", {

  expect_error(
    Int(1L) %>sv% Dbl_(c(1, 1))
  )

  expect_error(
    Dbl(1) %>sv% Int_(c(1L, 1L))
  )

})

test_that("%>sv% throws if inputs are base types", {

  expect_error(
    1 %>sv% c(1, 2)
  )

  expect_error(
    1L %>sv% c(1L, 2L)
  )

})

#-- VECTOR SCALAR

test_operator_V_S(`%>vs%`)

test_that("%>vs% return value is equal to length of vector", {

  scalar <- Int(1L)
  vector_a <- Int_(1L)
  vector_b <- Int_(c(1L, 2L))
  vector_c <- Int_(c(1L, 2L, 3L))

  expect_equal(
    length(vector_a),
    length(vector_a %>vs% scalar)
  )

  expect_equal(
    length(vector_b),
    length(vector_b %>vs% scalar)
  )

  expect_equal(
    length(vector_c),
    length(vector_c %>vs% scalar)
  )

})

test_that("%>vs% return value is Lgl_", {

  x <- Int_(1L)
  y <- Int(1L)

  expect_true(
    (x %>vs% y) %isa% Lgl_
  )

})

test_that("%>vs% returns correct values", {

  expect_equal(
    Int_(c(1L, 1L)) %>vs% Int(1L),
    Lgl_(c(FALSE, FALSE))
  )

  expect_equal(
    Dbl_(c(1, 1)) %>vs% Dbl(1),
    Lgl_(c(FALSE, FALSE))
  )

})

test_that("%>vs% throws if inputs are not numbers", {

  expect_error(
    Lgl_(TRUE) %>vs% Lgl(TRUE)
  )

  expect_error(
    Chr_("a") %>vs% Chr("b")
  )

})

test_that("%>vs% throws if an Int and Dbl are compared", {

  expect_error(
    Dbl_(c(1, 1)) %>vs% Int(1L)
  )

  expect_error(
    Int_(c(1L, 1L)) %>vs% Dbl(1)
  )

})

test_that("%>vs% throws if inputs are base types", {

  expect_error(
    c(1, 2) %>vs% 1
  )

  expect_error(
    c(1L, 2L) %>vs% 1L
  )

})
