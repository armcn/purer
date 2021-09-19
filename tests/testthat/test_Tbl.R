test_that(
  "
  Given Tbl should only convert a tibble to a Tbl class
  and throw with other inputs

  When the input is not a tibble

  Then Tbl should throw
  ",
{

  tbl_valid <-
    tibble::tibble(
      a = 1
    )

  types_valid <-
    list(
      a = "Int_"
    )

  typed_tbl <-
    Tbl(
      .x = tbl_valid
    , .types = types_valid
    )

  expect_is(
    typed_tbl
  , "Tbl"
  )

  expect_error(
    Tbl(
      .x = as.data.frame(tbl_valid)
    , .types = types_valid
    )
  )

  expect_error(
    Tbl(
      .x = as.list(tbl_valid)
    , .types = types_valid
    )
  )

})

test_that(
  "
  Given Tbl should only convert tibbles with columns
  that can covert to pure vectors

  When any columns can't be converted to pure vectors

  Then Tbl should throw
  "
, {

  tbl_valid <-
    tibble::tibble(
      a = 1
    , b = 1L
    , c = "A"
    , d = TRUE
    )

  types_valid <-
    list(
      a = "Dbl_"
    , b = "Int_"
    , c = "Chr_"
    , d = "Lgl_"
    )

  tbl_invalid_int <-
    tibble::tibble(
      a = "A"
    , b = NA_integer_
    )

  types_invalid_int <-
    list(
      a = "Chr_"
    , b = "Int_"
    )

  tbl_invalid_dbl <-
    tibble::tibble(
      a = "A"
    , b = NA_real_
    )

  types_invalid_dbl <-
    list(
      a = "Chr_"
    , b = "Dbl_"
    )

  tbl_invalid_chr <-
    tibble::tibble(
      a = 1
    , b = NA_character_
    )

  types_invalid_chr <-
    list(
      a = "Int_"
    , b = "Chr_"
    )

  tbl_invalid_lgl <-
    tibble::tibble(
      a = "A"
    , b = NA
    )

  types_invalid_lgl <-
    list(
      a = "Chr_"
    , b = "Lgl_"
    )

  expect_is(
    Tbl(
      .x = tbl_valid
    , .types = types_valid
    )
  , "Tbl"
  )

  expect_error(
    Tbl(
      .x = tbl_invalid_int
    , .types = types_invalid_int
    )
  )

  expect_error(
    Tbl(
      .x = tbl_invalid_dbl
    , .types = types_invalid_dbl
    )
  )

  expect_error(
    Tbl(
      .x = tbl_invalid_chr
    , .types = types_invalid_chr
    )
  )

  expect_error(
    Tbl(
      .x = tbl_invalid_lgl
    , .types = types_invalid_lgl
    )
  )

})

test_that(
  "
  Given that the argument .types must contain all of the columns
  in the tibble

  When the list has the wrong names

  Then Tbl should throw
  "
, {

  tbl_valid <-
    tibble::tibble(
      a = 1
    , b = 2L
    )

  types_valid <-
    list(
      a = "Dbl_"
    , b = "Int_"
    )

  types_invalid_less <-
    list(
      a = "Dbl_"
    )

  types_invalid_more <-
    list(
      a = "Dbl_"
    , b = "Int_"
    , c = "Chr_"
    )

  types_invalid_different <-
    list(
      list(
        a = "Int_"
      , b = "Int_"
      )
    )

  expect_is(
    Tbl(
      .x = tbl_valid
    , .types = types_valid
    )
  , "Tbl"
  )

  expect_error(
    Tbl(
      .x = tbl_valid
    , .types = types_invalid_less
    )
  )

  expect_error(
    Tbl(
      .x = tbl_valid
    , .types = types_invalid_more
    )
  )

  expect_error(
    Tbl(
      .x = tbl_valid
    , .types = types_invalid_different
    )
  )

})

test_that(
  "
  Given that the argument .types must contain only
  pure vector conversion function names

  When the list has invalid or scalar function names

  Then Tbl should throw
  "
, {

  tbl_valid <-
    tibble::tibble(
      a = 1
    , b = 2L
    )

  types_valid <-
    list(
      a = "Dbl_"
    , b = "Int_"
    )

  types_invalid_scalar <-
    list(
      a = "Dbl"
    , b = "Int_"
    )

  types_invalid_random <-
    list(
      a = "sdfd"
    , b = "Int_"
    )

  expect_is(
    Tbl(
      .x = tbl_valid
    , .types = types_valid
    )
  , "Tbl"
  )

  expect_error(
    Tbl(
      .x = tbl_valid
    , .types = types_invalid_scalar
    )
  )

  expect_error(
    Tbl(
      .x = tbl_valid
    , .types = types_invalid_random
    )
  )

})

