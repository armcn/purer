#' @export pure_min
pure_min(.x) %::% N : S
pure_min(.x) %as% {

  type <- pure_type(.x)

  .x |>
    drop_pure_type() |>
    min() |>
    set_pure_type_scalarize(type)

}

#' @export pure_min_
pure_min_(.x) %::% N : V
pure_min_(.x) %as% {

  val <- pure_min(.x)

  type <- pure_type(val)

  set_pure_type_vectorize(val, type)

}

#' @export pure_max
pure_max(.x) %::% N : S
pure_max(.x) %as% {

  type <- pure_type(.x)

  .x |>
    drop_pure_type() |>
    max() |>
    set_pure_type_scalarize(type)

}

#' @export pure_max_
pure_max_(.x) %::% N : V
pure_max_(.x) %as% {

  val <- pure_max(.x)

  type <- pure_type(val)

  set_pure_type_vectorize(val, type)

}

#' @export pure_mean
pure_mean(.x) %::% N : Dbl
pure_mean(.x) %as% {

  .x |>
    drop_pure_type() |>
    mean() |>
    Dbl()

}

#' @export pure_mean_
pure_mean_(.x) %::% N : Dbl_
pure_mean_(.x) %as% {

  val <- pure_mean(.x)

  type <- pure_type(val)

  set_pure_type_vectorize(val, type)

}

#' @export pure_median
pure_median(.x) %::% N : Dbl
pure_median(.x) %as% {

  .x |>
    drop_pure_type() |>
    median() |>
    Dbl()

}

#' @export pure_median_
pure_median_(.x) %::% N : Dbl_
pure_median_(.x) %as% {

  val <- pure_median(.x)

  type <- pure_type(val)

  set_pure_type_vectorize(val, type)

}


