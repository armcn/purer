#' Integer scalar type constructor
#'
#' @name Int
#' @param .x An integer scalar
#' @return An object with type \code{Int}
#' @examples
#' Int(1L)
#' @export Int
Int(.x) %when% {

  int_convertable(.x)

} %as% {

  if (.x %isa% Int) .x
  else .x |>
    as.integer() |>
    N() |>
    S() |>
    set_kind("Int")

}

#' @export
print.Int <- function(.x) {

  .x |>
    format(big.mark = " ") |>
    paste0(" : Int") |>
    print.default()

}

#' Integer vector type constructor
#'
#' @name Int_
#' @param .x An integer vector
#' @return An object with type \code{Int_}
#' @examples
#' Int_(1L)
#' @export Int_
Int_(.x) %when% {

  int_convertable(.x)

} %as% {

  if (.x %isa% Int_) .x
  else .x |>
    as.integer() |>
    N() |>
    V() |>
    set_kind("Int")

}

#' @export
print.Int_ <- function(.x) {

  .x |>
    map(\(x) format(x, big.mark = " ")) |>
    paste0(collapse = ", ") |>
    paste0(" : Vector Int") |>
    print.default()

}

int_convertable(.x) %as% {

  if (.x %isa% Int || .x %isa% Int_) {
    return(.x)
  }
  is.numeric(.x) &
  !is.na(.x) &
  is.null(attributes(.x)) &
  (.x %% 1 == 0) &
  abs(.x) <= .Machine$integer.max

}

#' Double scalar type constructor
#'
#' @name Dbl
#' @param .x A double vector
#' @return An object with type \code{Dbl}
#' @examples
#' Dbl(1)
#' @export Dbl
Dbl(.x) %when% {

  dbl_convertable(.x)

} %as% {

  if (.x %isa% Dbl) .x
  else .x |>
    as.double() |>
    N() |>
    S() |>
    set_kind("Dbl")

}

set_kind(.x, .kind) %as% {

  attr(.x, "kind") <- .kind

  .x

}

#' Double vector type constructor
#'
#' @name Dbl_
#' @param .x A double vector
#' @return An object with type \code{Dbl_}
#' @examples
#' Dbl_(c(1, 2))
#' @export Dbl_
Dbl_(.x) %when% {

  dbl_convertable(.x)

} %as% {

  if (.x %isa% Dbl_) return(.x)

  if (.x %isa% Int) {
    x <- as.double(.x)
  } else {
    x <- .x
  }

  x <- N(V(x))

  x@kind <- "Dbl"

  x

}

dbl_convertable(.x) %as% {

  if (isTRUE(.x@kind == "Dbl")) return(TRUE)

  is.numeric(.x) &
  !is.na(.x) &
  !is.nan(.x) &
  !is.infinite(.x) &
  is.null(attributes(.x))

}

#' Character scalar type constructor
#'
#' @name Chr
#' @param .x A character vector
#' @return An object with type \code{Chr}
#' @examples
#' Chr("a")
#' @export Chr
Chr(.x) %when% {

  chr_convertable(.x)

} %as% {

  if (.x %isa% Chr) return(.x)

  x <- S(.x)

  x@kind <- "Chr"

  x

}

#' Character vector type constructor
#'
#' @name Chr_
#' @param .x A character vector
#' @return An object with type \code{Chr_}
#' @examples
#' Chr_(c("a", "b"))
#' @export Chr_
Chr_(.x) %when% {

  chr_convertable(.x)

} %as% {

  if (.x %isa% Chr_) return(.x)

  x <- V(.x)

  x@kind <- "Chr"

  x

}

chr_convertable(.x) %as% {

  if (isTRUE(.x@kind == "Chr")) return(TRUE)

  is.character(.x) &
  !is.na(.x) &
  is.null(attributes(.x))

}

#' Logical scalar type constructor
#'
#' @name Lgl
#' @param .x A logical vector
#' @return An object with type \code{Lgl}
#' @examples
#' Lgl("a")
#' @export Lgl
Lgl(.x) %when% {

  lgl_convertable(.x)

} %as% {

  if (.x %isa% Lgl) return(.x)

  x <- S(.x)

  x@kind <- "Lgl"

  x

}

#' Logical vector type constructor
#'
#' @name Lgl_
#' @param .x A logical vector
#' @return An object with type \code{Lgl_}
#' @examples
#' Lgl_(c(TRUE, FALSE))
#' @export Lgl_
Lgl_(.x) %when% {

  lgl_convertable(.x)

} %as% {

  if (.x %isa% Lgl_) return(.x)

  x <- V(.x)

  x@kind <- "Lgl"

  x

}

lgl_convertable(.x) %as% {

  if (isTRUE(.x@kind == "Lgl")) return(TRUE)

  is.logical(.x) &
  !is.na(.x) &
  is.null(attributes(.x))

}



#' Pure tibble type constructor
#'
#' @name Tbl
#' @param .x A tibble
#' @param .types A list of column types.
#'   List names are the column names of the tibble
#'   and the list values are the pure vector type.
#' @return An object with type \code{Tbl}
#' @examples
#' Tbl(
#'   tibble::tibble(
#'     a = c(1, 2),
#'     b = c("a", "b")
#'   ),
#'   .types = list(
#'     a = "Dbl_",
#'     b = "Chr_"
#'   )
#' )
#' @export Tbl
Tbl(.x, .types) %::% tbl_df : list : .
Tbl(.x, .types) %when% {

  identical(
    sort(names(.x))
  , sort(names(.types))
  )

  all(
    .types %in%
      pure_vectors()
  )

} %as% {

  .x |>
    imap_dfc(
      \(.col, .id)
        .types |>
          pluck(.id) |>
          exec(.col)
    )

}

S(.x) %when% {

  length(.x) == 1

} %as% {

  .x

}

V(.x) %when% {

  length(.x) >= 1

} %as% {

  .x

}

N(.x) %when% {

  is.numeric(.x)

} %as% {

  .x

}

