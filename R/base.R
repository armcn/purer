#' @export pure_c
pure_c(...) %::% ... : V
pure_c(...) %when% {

  types_equal(...)

} %as% {

  val <- c(...)

  type <- ... |>
    list() |>
    first() |>
    pure_type()

  set_pure_type_vectorize(val, type)

}
