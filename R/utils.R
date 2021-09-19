is_pure_vector(.x) %::% a : Lgl
is_pure_vector(.x) %as% {

  Lgl(
    .x %isa% Int_ ||
    .x %isa% Dbl_ ||
    .x %isa% Chr_ ||
    .x %isa% Lgl_
  )

}

is_pure_scalar(.x) %::% a : Lgl
is_pure_scalar(.x) %as% {

  Lgl(
    .x %isa% Int ||
    .x %isa% Dbl ||
    .x %isa% Chr ||
    .x %isa% Lgl
  )

}

is_pure(.x) %::% a : Lgl
is_pure(.x) %as% {

  Lgl(
    is_pure_scalar(.x) ||
    is_pure_vector(.x)
  )

}

pure_type(.x) %::% a : Chr
pure_type(.x) %when% {

  is_pure(.x)

} %as% {

  Chr(
    class(.x)[1]
  )

}

drop_pure_type(.x) %as% {

  x <- unclass(.x)

  attr(x, "kind") <- NULL

  x

}

set_pure_type(.x, .type) %::% . : character : .
set_pure_type(.x, .type) %as% {

  type_functions <- list(
    "Int" = Int,
    "Int_" = Int_,
    "Dbl" = Dbl,
    "Dbl_" = Dbl_,
    "Chr" = Chr,
    "Chr_" = Chr_,
    "Lgl" = Lgl,
    "Lgl_" = Lgl_
  )

  .x |>
    drop_pure_type() |>
    pluck(type_functions, .type)()

}

set_pure_type_vectorize(.x, .type) %::% . : character : V
set_pure_type_vectorize(.x, .type) %as% {

  type_converter <- list(
    "Int" = Int_,
    "Int_" = Int_,
    "Dbl" = Dbl_,
    "Dbl_" = Dbl_,
    "Chr" = Chr_,
    "Chr_" = Chr_,
    "Lgl" = Lgl_,
    "Lgl_" = Lgl_
  )

  .x |>
    drop_pure_type() |>
    pluck(type_converter, .type)()

}

set_pure_type_scalarize(.x, .type) %::% . : character : S
set_pure_type_scalarize(.x, .type) %as% {

  type_converter <- list(
    "Int" = Int,
    "Int_" = Int,
    "Dbl" = Dbl,
    "Dbl_" = Dbl,
    "Chr" = Chr,
    "Chr_" = Chr,
    "Lgl" = Lgl,
    "Lgl_" = Lgl
  )

  .x |>
    drop_pure_type() |>
    pluck(type_converter, .type)()

}

types_equal(...) %::% ... : Lgl
types_equal(...) %as% {

  ... |>
    list() |>
    map(pure_type) |>
    n_distinct() |>
    {\(x) x == 1}() |>
    Lgl()

}

kinds_equal(...) %::% ... : Lgl
kinds_equal(...) %as% {

  ... |>
    list() |>
    map(\(x) attr(x, "kind")) |>
    n_distinct() |>
    {\(x) x == 1}() |>
    Lgl()

}

lengths_equal(.x, .y) %::% a : a : Lgl
lengths_equal(.x, .y) %as% {

  Lgl(
    length(.x) == length(.y)
  )

}

n_distinct <- compose(length, unique)

pure_vectors() %as% c("Int_", "Dbl_", "Chr_", "Lgl_")

random_100_integers() %as% {

  max_value <-
    1000000

  negative_integers <-
    runif(49, min = -max_value, max = 0) |>
    as.integer()

  positive_integers <-
    runif(50, min = 0, max = max_value) |>
    as.integer()

  c(negative_integers, 0, positive_integers)

}

random_n_integers() %as% {

  max_value <-
    1000000

  n_integers <-
    runif(1, 1, 100)

  runif(n_integers, -max_value, max_value) |>
    as.integer()

}

random_100_whole_doubles() %as% {

  max_value <-
    1000000

  negative_doubles <-
    runif(49, -max_value, 0) |>
    round()

  positive_doubles <-
    runif(50, 0, max_value) |>
    round()

  c(negative_doubles, 0, positive_doubles) |>
    as.list()

}

random_n_whole_doubles() %as% {

  max_value <-
    1000000

  n_doubles <-
    runif(1, 1, 100)

  runif(n_doubles, -max_value, max_value) |>
    round() |>
    as.list()

}

random_100_fractional_doubles() %as% {

  max_value <-
    1000000

  negative_doubles <-
    runif(49, -max_value, 0) |>
    round() |>
    {\(x) x + runif(1, 0.01, 0.99)}()

  positive_doubles <-
    runif(50, min = 0, max = max_value) |>
    round() |>
    {\(x) x + runif(1, 0.01, 0.99)}()

  c(negative_doubles, positive_doubles) |>
    as.list()

}

