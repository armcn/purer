#' Add two numbers together
#'
#' @name pure_add
#' @param .x A number
#' @param .y A number
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_add(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_add(Dbl(1), Dbl_(c(1, 2)), .types = Chr("sv"))
#' @family add functions
#' @export pure_add
pure_add(.x, .y, .types = Chr("ss")) %::% N : N : Chr : N
pure_add(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  add_funs <- list(
    "ss" = `%+ss%`,
    "vv" = `%+vv%`,
    "sv" = `%+sv%`,
    "vs" = `%+vs%`
  )

  pluck(add_funs, .types)(.x, .y)

}

#' @rdname pure_add
#' @export
add <- pure_add

#' Add two scalar numbers
#'
#' @name %+ss%
#' @param .x A number
#' @param .y A number
#' @return A scalar with the same type as input
#' @examples
#' Dbl(1) %+ss% Dbl(1)
#' Int(1L) %+ss% Int(1L)
#' @family add functions
#' @export `%+ss%`
`%+ss%`(.x, .y) %::% S : S : S
`%+ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x + .y

  set_pure_type(val, type)

}

#' Add two number vectors
#'
#' @name %+vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %+vv% Dbl_(c(1, 2))
#' Int_(c(1L, 2L)) %+vv% Int_(c(1L, 2L))
#' @family add functions
#' @export `%+vv%`
`%+vv%`(.x, .y) %::% V : V : V
`%+vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x + .y

  set_pure_type(val, type)

}

#' Add a number scalar to a number vector
#'
#' @name %+sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %+sv% Dbl_(c(1, 2))
#' Int(1) %+sv% Int_(c(1L, 2L))
#' @family add functions
#' @export `%+sv%`
`%+sv%`(.x, .y) %::% S : V : V
`%+sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.y)

  val <- .x + .y

  set_pure_type(val, type)

}
#' Add a number vector to a number scalar
#'
#' @name %+vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %+vs% Dbl(1)
#' Int_(c(1L, 2L)) %+vs% Int(1L)
#' @family add functions
#' @export `%+vs%`
`%+vs%`(.x, .y) %::% V : S : V
`%+vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x + .y

  set_pure_type(val, type)

}

#' Subtract two numbers
#'
#' @name pure_subtract
#' @param .x A number
#' @param .y A number
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_subtract(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_subtract(Dbl(1), Dbl_(c(1, 2)), .types = Chr("sv"))
#' @family subtract functions
#' @export pure_subtract
pure_subtract(.x, .y, .types = Chr("ss")) %::% N : N : Chr : N
pure_subtract(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  subtract_funs <- list(
    "ss" = `%-ss%`,
    "vv" = `%-vv%`,
    "sv" = `%-sv%`,
    "vs" = `%-vs%`
  )

  pluck(subtract_funs, .types)(.x, .y)

}

#' @rdname pure_subtract
#' @export
subt <- pure_subtract

#' Subtract two scalar numbers
#'
#' @name %-ss%
#' @param .x A number
#' @param .y A number
#' @return A scalar with the same type as input
#' @examples
#' Dbl(1) %-ss% Dbl(1)
#' Int(1L) %-ss% Int(1L)
#' @family subtract functions
#' @export `%-ss%`
`%-ss%`(.x, .y) %::% S : S : S
`%-ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x - .y

  set_pure_type(val, type)

}

#' Subtract two number vectors
#'
#' @name %-vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %-vv% Dbl(1)
#' Int(1L) %-vv% Int(1L)
#' @family subtract functions
#' @export `%-vv%`
`%-vv%`(.x, .y) %::% V : V : V
`%-vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x - .y

  set_pure_type(val, type)

}

#' Subtract a number vector from a number scalar
#'
#' @name %-sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %-sv% Dbl_(c(1, 2))
#' Int(1) %-sv% Int_(c(1L, 2L))
#' @family subtract functions
#' @export `%-sv%`
`%-sv%`(.x, .y) %::% S : V : V
`%-sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.y)

  val <- .x - .y

  set_pure_type(val, type)

}

#' Subtract a number scalar from a number vector
#'
#' @name %-vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %-vs% Dbl(1)
#' Int_(c(1L, 2L)) %-vs% Int(1L)
#' @family subtract functions
#' @export `%-vs%`
`%-vs%`(.x, .y) %::% V : S : V
`%-vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x - .y

  set_pure_type(val, type)

}

#' Multiply two numbers
#'
#' @name pure_multiply
#' @param .x A number
#' @param .y A number
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_multiply(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_multiply(Dbl(1), Dbl_(c(1, 2)), .types = Chr("sv"))
#' @family multiply functions
#' @export pure_multiply
pure_multiply(.x, .y, .types = Chr("ss")) %::% N : N : Chr : N
pure_multiply(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  multiply_funs <- list(
    "ss" = `%-ss%`,
    "vv" = `%-vv%`,
    "sv" = `%-sv%`,
    "vs" = `%-vs%`
  )

  pluck(multiply_funs, .types)(.x, .y)

}

#' @rdname pure_multiply
#' @export
mult <- pure_multiply

#' Multiply two scalar numbers
#'
#' @name %*ss%
#' @param .x A number
#' @param .y A number
#' @return A scalar with the same type as input
#' @examples
#' Dbl(1) %*ss% Dbl(1)
#' Int(1L) %*ss% Int(1L)
#' @family multiply functions
#' @export `%*ss%`
`%*ss%`(.x, .y) %::% S : S : S
`%*ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x * .y

  set_pure_type(val, type)

}

#' Multiply two number vectors
#'
#' @name %*vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %*vv% Dbl(1)
#' Int(1L) %*vv% Int(1L)
#' @family multiply functions
#' @export `%*vv%`
`%*vv%`(.x, .y) %::% V : V : V
`%*vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x * .y

  set_pure_type(val, type)

}

#' Multiply a number vector from a number scalar
#'
#' @name %*sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %*sv% Dbl_(c(1, 2))
#' Int(1) %*sv% Int_(c(1L, 2L))
#' @family multiply functions
#' @export `%*sv%`
`%*sv%`(.x, .y) %::% S : V : V
`%*sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.y)

  val <- .x * .y

  set_pure_type(val, type)

}

#' Multiply a number scalar from a number vector
#'
#' @name %*vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %*vs% Dbl(1)
#' Int_(c(1L, 2L)) %*vs% Int(1L)
#' @family multiply functions
#' @export `%*vs%`
`%*vs%`(.x, .y) %::% V : S : V
`%*vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x * .y

  set_pure_type(val, type)

}

#' Divide two numbers
#'
#' @name pure_divide
#' @param .x A number
#' @param .y A number
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_divide(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_divide(Dbl(1), Dbl_(c(1, 2)), .types = Chr("sv"))
#' @family divide functions
#' @export pure_divide
pure_divide(.x, .y, .types = Chr("ss")) %::% N : N : Chr : N
pure_divide(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  divide_funs <- list(
    "ss" = `%/ss%`,
    "vv" = `%/vv%`,
    "sv" = `%/sv%`,
    "vs" = `%/vs%`
  )

  pluck(divide_funs, .types)(.x, .y)

}

#' @rdname pure_divide
#' @export
divi <- pure_divide

#' Divide a scalar number by a scalar number
#'
#' @name %/ss%
#' @param .x A number
#' @param .y A number
#' @return A scalar with the same type as input
#' @examples
#' Dbl(1) %/ss% Dbl(1)
#' Int(1L) %/ss% Int(1L)
#' @family divide functions
#' @export `%/ss%`
`%/ss%`(.x, .y) %::% S : S : S
`%/ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  .y != 0
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x / .y

  set_pure_type(val, type)

}

#' Divide a number vector by a number vector
#'
#' @name %/vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A vector with the same kind as inputs
#' @examples
#' Dbl_(c(1, 2)) %/vv% Dbl_(c(1, 2))
#' Int_(c(1, 2)) %/vv% Int_(c(1, 2))
#' @family divide functions
#' @export `%/vv%`
`%/vv%`(.x, .y) %::% V : V : V
`%/vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  all(.y != 0)
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x / .y

  set_pure_type(val, type)

}

#' Divide a number scalar by a number vector
#'
#' @name %/sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A vector with the same kind as inputs
#' @examples
#' Dbl(1) %/sv% Dbl_(c(1, 2))
#' Int(1) %/sv% Int_(c(1, 2))
#' @family divide functions
#' @export `%/sv%`
`%/sv%`(.x, .y) %::% S : V : V
`%/sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  all(.y != 0)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.y)

  val <- .x / .y

  set_pure_type(val, type)

}

#' Divide a number vector by a number scalar
#'
#' @name %/vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A vector with the same kind as inputs
#' @examples
#' Dbl_(c(1, 2)) %/vs% Dbl(1)
#' Int_(c(1L, 2L)) %/vs% Int(1L)
#' @family divide functions
#' @export `%/vs%`
`%/vs%`(.x, .y) %::% V : S : V
`%/vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  .y != 0
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x / .y

  set_pure_type(val, type)

}

#' Get modulo of two numbers
#'
#' @name pure_modulo
#' @param .x A number
#' @param .y A number
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_modulo(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_modulo(Dbl(1), Dbl_(c(1, 2)), .types = Chr("sv"))
#' @family modulo functions
#' @export pure_modulo
pure_modulo(.x, .y, .types) %::% N : N : Chr : N
pure_modulo(.x, .y, .types) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  modulo_funs <- list(
    "ss" = `%modss%`,
    "vv" = `%modvv%`,
    "sv" = `%modsv%`,
    "vs" = `%modvs%`
  )

  pluck(modulo_funs, .types)(.x, .y)

}

#' @rdname pure_modulo
#' @export
modu <- pure_modulo

#' Get modulo of two scalar numbers
#'
#' @name %modss%
#' @param .x A number
#' @param .y A number
#' @return A scalar with the same type as input
#' @examples
#' Dbl(1) %modss% Dbl(1)
#' Int(1L) %modss% Int(1L)
#' @family modulo functions
#' @export `%modss%`
`%modss%`(.x, .y) %::% S : S : S
`%modss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  .y != 0
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x %% .y

  set_pure_type(val, type)

}

#' Get modulo of two number vectors
#'
#' @name %modvv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %modvv% Dbl_(c(1, 2))
#' Int_(c(1L, 2L)) %modvv% Int_(c(1L, 2L))
#' @family modulo functions
#' @export `%modvv%`
`%modvv%`(.x, .y) %::% V : V : V
`%modvv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  all(.y != 0)
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x %% .y

  set_pure_type(val, type)

}

#' Get modulo of a number scalar and a number vector
#'
#' @name %modsv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A vector with the same type as input
#' @examples
#' Dbl(1) %modsv% Dbl_(c(1, 2))
#' Int(1) %modsv% Int_(c(1L, 2L))
#' @family add functions
#' @export `%modsv%`
`%modsv%`(.x, .y) %::% S : V : V
`%modsv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  all(.y != 0)
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.y)

  val <- .x %% .y

  set_pure_type(val, type)

}
#' Get modulo of a number vector and a number scalar
#'
#' @name %modvs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A vector with the same type as input
#' @examples
#' Dbl_(c(1, 2)) %modvs% Dbl(1)
#' Int_(c(1L, 2L)) %modvs% Int(1L)
#' @family modulo functions
#' @export `%modvs%`
`%modvs%`(.x, .y) %::% V : S : V
`%modvs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  .y != 0
  kinds_equal(.x, .y)

} %as% {

  type <- pure_type(.x)

  val <- .x %% .y

  set_pure_type(val, type)

}

#' Check equality of two scalars or vectors
#'
#' @name pure_equal
#' @param .x A scalar or vector
#' @param .y A scalar or vector
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_equal(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_equal(Chr("a"), Chr_(c("a", "b")), .types = Chr("sv"))
#' @family equal functions
#' @export pure_equal
pure_equal(.x, .y, .types) %::% . : . : Chr : .
pure_equal(.x, .y, .types) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  equal_funs <- list(
    "ss" = `%==ss%`,
    "vv" = `%==vv%`,
    "sv" = `%==sv%`,
    "vs" = `%==vs%`
  )

  pluck(equal_funs, .types)(.x, .y)

}

#' @rdname pure_equal
#' @export
equal <- pure_equal

#' Check equality of two scalars
#'
#' @name %==ss%
#' @param .x A scalar
#' @param .y A scalar
#' @return A logical scalar
#' @examples
#' Int(1L) %==ss% Int(1L)
#' @family equal functions
#' @export `%==ss%`
`%==ss%`(.x, .y) %::% S : S : Lgl
`%==ss%`(.x, .y) %when% {

  kinds_equal(.x, .y)

} %as% {

  Lgl(.x == .y)

}

#' Check equality of two vectors
#'
#' @name %==vv%
#' @param .x A vector
#' @param .y A vector
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %==ss% Int_(c(1L, 2L))
#' @family equal functions
#' @export `%==vv%`
`%==vv%`(.x, .y) %::% V : V : Lgl_
`%==vv%`(.x, .y) %when% {

  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x == .y)

}

#' Check equality of a scalar and vector
#'
#' @name %==sv%
#' @param .x A scalar
#' @param .y A vector
#' @return A logical vector
#' @examples
#' Int(1L) %==ss% Int_(c(1L, 2L))
#' @family equal functions
#' @export `%==sv%`
`%==sv%`(.x, .y) %::% S : V : Lgl_
`%==sv%`(.x, .y) %when% {

  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x == .y)

}

#' Check equality of a vector and scalar
#'
#' @name %==vs%
#' @param .x A vector
#' @param .y A scalar
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %==ss% Int(1L)
#' @family equal functions
#' @export `%==vs%`
`%==vs%`(.x, .y) %::% V : S : Lgl_
`%==vs%`(.x, .y) %when% {

  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x == .y)

}

#' Greater than number comparison
#'
#' @name pure_gt
#' @param .x A scalar or vector
#' @param .y A scalar or vector
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_gt(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_gt(Dbl(1), Dbl(1), .types = Chr("ss"))
#' @family gt functions
#' @export pure_gt
pure_gt(.x, .y, .types = Chr("ss")) %::% N : N : Chr : .
pure_gt(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  gt_funs <- list(
    "ss" = `%>ss%`,
    "vv" = `%>vv%`,
    "sv" = `%>sv%`,
    "vs" = `%>vs%`
  )

  pluck(gt_funs, .types)(.x, .y)

}

#' @rdname pure_gt
#' @export
gt <- pure_gt

#' Greater than scalar number comparison
#'
#' @name %>ss%
#' @param .x A number scalar
#' @param .y A number scalar
#' @return A logical scalar
#' @examples
#' Int(1L) %>ss% Int(1L)
#' @family gt functions
#' @export `%>ss%`
`%>ss%`(.x, .y) %::% S : S : Lgl
`%>ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl(.x > .y)

}

#' Greater than vector number comparison
#'
#' @name %>vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %>vv% Int_(c(1L, 1L))
#' @family gt functions
#' @export `%>vv%`
`%>vv%`(.x, .y) %::% V : V : Lgl_
`%>vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x > .y)

}

#' Greater than scalar to vector number comparison
#'
#' @name %>sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int(1L) %>sv% Int_(c(1L, 2L))
#' @family gt functions
#' @export `%>sv%`
`%>sv%`(.x, .y) %::% S : V : Lgl_
`%>sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x > .y)

}

#' Greater than vector to scalar number comparison
#'
#' @name %>vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %>vs% Int(1L)
#' @family gt functions
#' @export `%>vs%`
`%>vs%`(.x, .y) %::% V : S : Lgl_
`%>vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x > .y)

}

#' Less than number comparison
#'
#' @name pure_lt
#' @param .x A scalar or vector
#' @param .y A scalar or vector
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_lt(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_lt(Dbl(1), Dbl(1), .types = Chr("ss"))
#' @family lt functions
#' @export pure_lt
pure_lt(.x, .y, .types = Chr("ss")) %::% N : N : Chr : .
pure_lt(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  lt_funs <- list(
    "ss" = `%<ss%`,
    "vv" = `%<vv%`,
    "sv" = `%<sv%`,
    "vs" = `%<vs%`
  )

  pluck(lt_funs, .types)(.x, .y)

}

#' @rdname pure_lt
#' @export
lt <- pure_lt

#' Less than scalar number comparison
#'
#' @name %<ss%
#' @param .x A number scalar
#' @param .y A number scalar
#' @return A logical scalar
#' @examples
#' Int(1L) %<ss% Int(1L)
#' @family lt functions
#' @export `%<ss%`
`%<ss%`(.x, .y) %::% S : S : Lgl
`%<ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl(.x < .y)

}

#' Less than vector number comparison
#'
#' @name %<vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %<vv% Int_(c(1L, 1L))
#' @family lt functions
#' @export `%<vv%`
`%<vv%`(.x, .y) %::% V : V : Lgl_
`%<vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x < .y)

}

#' Less than scalar to vector number comparison
#'
#' @name %<sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int(1L) %<sv% Int_(c(1L, 2L))
#' @family lt functions
#' @export `%<sv%`
`%<sv%`(.x, .y) %::% S : V : Lgl_
`%<sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x < .y)

}

#' Less than vector to scalar number comparison
#'
#' @name %<vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %<vs% Int(1L)
#' @family lt functions
#' @export `%<vs%`
`%<vs%`(.x, .y) %::% V : S : Lgl_
`%<vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x < .y)

}

#' Greater than or equal number comparison
#'
#' @name pure_gte
#' @param .x A scalar or vector
#' @param .y A scalar or vector
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_gte(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_gte(Dbl(1), Dbl(1), .types = Chr("ss"))
#' @family gte functions
#' @export pure_gte
pure_gte(.x, .y, .types = Chr("ss")) %::% N : N : Chr : .
pure_gte(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  gte_funs <- list(
    "ss" = `%>=ss%`,
    "vv" = `%>=vv%`,
    "sv" = `%>=sv%`,
    "vs" = `%>=vs%`
  )

  pluck(gte_funs, .types)(.x, .y)

}

#' @rdname pure_gte
#' @export
gte <- pure_gte

#' Greater than or equal scalar number comparison
#'
#' @name %>=ss%
#' @param .x A number scalar
#' @param .y A number scalar
#' @return A logical scalar
#' @examples
#' Int(1L) %>=ss% Int(1L)
#' @family gte functions
#' @export `%>=ss%`
`%>=ss%`(.x, .y) %::% S : S : Lgl
`%>=ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl(.x >= .y)

}

#' Greater than or equal vector number comparison
#'
#' @name %>=vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %>=vv% Int_(c(1L, 1L))
#' @family gte functions
#' @export `%>=vv%`
`%>=vv%`(.x, .y) %::% V : V : Lgl_
`%>=vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x >= .y)

}

#' Greater than or equal scalar to vector number comparison
#'
#' @name %>=sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int(1L) %>=sv% Int_(c(1L, 2L))
#' @family gte functions
#' @export `%>=sv%`
`%>=sv%`(.x, .y) %::% S : V : Lgl_
`%>=sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x >= .y)

}

#' Greater than or equal vector to scalar number comparison
#'
#' @name %>=vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %>=vs% Int(1L)
#' @family gte functions
#' @export `%>=vs%`
`%>=vs%`(.x, .y) %::% V : S : Lgl_
`%>=vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x >= .y)

}

#' Less than number comparison
#'
#' @name pure_lte
#' @param .x A scalar or vector
#' @param .y A scalar or vector
#' @return A scalar or a vector depending on \code{.types}
#' @examples
#' pure_lte(Int(1L), Int(1L), .types = Chr("ss"))
#' pure_lte(Dbl(1), Dbl(1), .types = Chr("ss"))
#' @family lte functions
#' @export pure_lte
pure_lte(.x, .y, .types = Chr("ss")) %::% N : N : Chr : .
pure_lte(.x, .y, .types = Chr("ss")) %when% {

  .types %in% c("ss", "vv", "sv", "vs")

} %as% {

  lte_funs <- list(
    "ss" = `%<=ss%`,
    "vv" = `%<=vv%`,
    "sv" = `%<=sv%`,
    "vs" = `%<=vs%`
  )

  pluck(lte_funs, .types)(.x, .y)

}

#' @rdname pure_lte
#' @export
lte <- pure_lte

#' Less than or equal scalar number comparison
#'
#' @name %<=ss%
#' @param .x A number scalar
#' @param .y A number scalar
#' @return A logical scalar
#' @examples
#' Int(1L) %<=ss% Int(1L)
#' @family lte functions
#' @export `%<=ss%`
`%<=ss%`(.x, .y) %::% S : S : Lgl
`%<=ss%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl(.x <= .y)

}

#' Less than or equal vector number comparison
#'
#' @name %<=vv%
#' @param .x A number vector
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %<=vv% Int_(c(1L, 1L))
#' @family lte functions
#' @export `%<vv%`
`%<=vv%`(.x, .y) %::% V : V : Lgl_
`%<=vv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  lengths_equal(.x, .y)
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x <= .y)

}

#' Less than or equal scalar to vector number comparison
#'
#' @name %<=sv%
#' @param .x A number scalar
#' @param .y A number vector
#' @return A logical vector
#' @examples
#' Int(1L) %<=sv% Int_(c(1L, 2L))
#' @family lte functions
#' @export `%<=sv%`
`%<=sv%`(.x, .y) %::% S : V : Lgl_
`%<=sv%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x <= .y)

}

#' Less than or equal vector to scalar number comparison
#'
#' @name %<=vs%
#' @param .x A number vector
#' @param .y A number scalar
#' @return A logical vector
#' @examples
#' Int_(c(1L, 2L)) %<=vs% Int(1L)
#' @family lte functions
#' @export `%<=vs%`
`%<=vs%`(.x, .y) %::% V : S : Lgl_
`%<=vs%`(.x, .y) %when% {

  .x %isa% N
  .y %isa% N
  kinds_equal(.x, .y)

} %as% {

  Lgl_(.x <= .y)

}

