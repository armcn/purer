Int_succeeds <-
  possibly(\(x) Int(x) %isa% Int, FALSE)

Int_throws <-
  negate(Int_succeeds)

expect_Int_succeeds <-
  compose(expect_true, Int_succeeds)

expect_Int_throws <-
  compose(expect_true, Int_throws)

test_that(
  "
  Given Int should only convert integers and doubles to Int,
  and the integers and doubles must not be missing or undefined,
  and any doubles must be whole numbers

  When the input is not a valid integer or double

  Then Int should throw
  "
, {

  gen_ints() |>
    forall(expect_Int_succeeds)

  gen_int_convertable_dbls() |>
    forall(expect_Int_succeeds)

  gen_fractional_dbls() |>
    forall(expect_Int_throws)

  gen_non_int_convertable() |>
    forall(expect_Int_throws)

})

test_that(
  "
  Given Int should only convert single-length vectors

  When the input has length > 1

  Then Int should throw
  "
, {

  gen_int_vecs(.min_len = 2) |>
    forall(expect_Int_throws)

  gen_int_convertable_dbl_vecs(.min_len = 2) |>
    forall(expect_Int_throws)

})

test_that(
  "
  Given Int should never return NA

  When the input is larger than the maximum integer size

  Then Int should throw
  "
, {

  largest_integer <-
    .Machine$integer.max

  gen.element(-largest_integer:largest_integer) |>
    forall(expect_Int_succeeds)

  gen.element(-1e6:-0.01) |>
    gen.and_then(\(x) as.double(x) - largest_integer) |>
    forall(expect_Int_throws)

  gen.element(0.01:1e6) |>
    gen.and_then(\(x) as.double(x) + largest_integer) |>
    forall(expect_Int_throws)

})
