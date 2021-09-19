Dbl_succeeds <-
  possibly(\(x) Dbl(x) %isa% Dbl, FALSE)

Dbl_throws <-
  negate(Dbl_succeeds)

expect_Dbl_succeeds <-
  compose(expect_true, Dbl_succeeds)

expect_Dbl_throws <-
  compose(expect_true, Dbl_throws)

test_that(
  "
  Given Dbl should only convert integers and doubles to Dbl,
  and the integers and doubles must not be missing or undefined

  When the input is not a valid integer or double

  Then Dbl should throw
  "
, {

  gen_dbls() |>
    forall(expect_Dbl_succeeds)

  gen_ints() |>
    forall(expect_Dbl_succeeds)

  gen_non_dbl_convertable() |>
    forall(expect_Dbl_throws)

})

test_that(
  "
  Given Dbl should only convert single-length vectors

  When the input has length > 1

  Then Dbl should throw
  "
, {

  gen_dbl_vecs(.min_len = 2) |>
    forall(expect_Dbl_throws)

  gen_int_vecs(.min_len = 2) |>
    forall(expect_Dbl_throws)

})

