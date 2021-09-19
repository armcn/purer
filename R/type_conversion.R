#' @export pure_int_to_dbl
pure_int_to_dbl(.x) %::% Int : Dbl
pure_int_to_dbl(.x) %as% {

  .x |>
    drop_pure_type() |>
    as.double() |>
    Dbl()

}

#' @export pure_int_to_dbl_
pure_int_to_dbl_(.x) %::% Int_ : Dbl_
pure_int_to_dbl_(.x) %as% {

  .x |>
    drop_pure_type() |>
    as.double() |>
    Dbl_()

}

#' @export pure_dbl_to_int
pure_dbl_to_int(.x) %::% Dbl : Int
pure_dbl_to_int(.x) %when% {

  .x |>
    modu(Dbl(1), .types = Chr("ss")) |>
    equal(Dbl(0), .types = Chr("ss"))

} %as% {

  .x |>
    drop_pure_type() |>
    as.integer() |>
    Int()

}

#' @export pure_dbl_to_int_
pure_dbl_to_int_(.x) %::% Dbl_ : Int_
pure_dbl_to_int_(.x) %when% {

  .x |>
    modu(Dbl(1)) |>
    equal(Dbl(0))

} %as% {

  .x |>
    drop_pure_type() |>
    as.integer() |>
    Int_()

}

# #' @export pure_int_to_lgl
# pure_int_to_lgl(.x) %::% Int : Lgl
# pure_int_to_lgl(.x) %when% {
#
#
#
# }
