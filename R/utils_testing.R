

#-- Single-length atomic vector generators


gen_ints() %as% {

  gen.element(-1e6:1e6) |>
    gen.choice(gen.element(0L))
}

gen_dbls() %as% {

  gen.unif(-1e6, 1e6) |>
    gen.choice(gen.element(0))

}

gen_chrs() %as% {

  32:126 |>
    as.raw() |>
    rawToChar() |>
    strsplit("") |>
    unlist() |>
    gen.element()

}

gen_lgls() %as% {

  gen.choice(TRUE, FALSE)

}



#-- Variable length atomic vector generators


gen_int_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_ints() |>
    gen.c(
      from = .min_len
    , to = .max_len
    )

}

gen_dbl_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_dbls() |>
    gen.c(
      from = .min_len
    , to = .max_len
    )

}

gen_chr_vecs(
  .min_len = 1
, .max_len = 100) %as% {

  gen_chrs() |>
    gen.c(
      from = .min_len
    , to = .max_len
    )

}

gen_lgl_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_lgls() |>
    gen.c(
      from = .min_len
    , to = .max_len
    )

}



#-- purer scalar type generators


gen_Ints() %as% {

  gen_ints() |>
    gen.and_then(Int)

}

gen_Dbls() %as% {

  gen_dbls() |>
    gen.and_then(Dbl)

}

gen_Chrs() %as% {

  gen_chrs() |>
    gen.and_then(Chr)
}

gen_Lgls() %as% {

  gen_lgls() |>
    gen.and_then(Lgl)

}



#-- purer vector type generators


gen_Int_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_int_vecs(
    .min_len = .min_len
  , .max_len = .max_len
  ) |>
    gen.and_then(Int_)

}

gen_Dbl_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_dbl_vecs(
    .min_len = .min_len
  , .max_len = .max_len
  ) |>
    gen.and_then(Dbl_)

}

gen_Chr_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_chr_vecs(
    .min_len = .min_len
  , .max_len = .max_len
  ) |>
    gen.and_then(Chr_)

}

gen_Lgl_vecs(
  .min_len = 1
, .max_len = 100
) %as% {

  gen_lgl_vecs(
    .min_len = .min_len
  , .max_len = .max_len
  ) |>
    gen.and_then(Lgl_)

}



#--




gen_int_convertable_dbls() %as% {

  gen_ints() |>
    gen.and_then(as.double)

}

gen_fractional_dbls() %as% {

  gen_ints() |>
    gen.and_then(\(x) x + runif(1, 0.01, 0.99))

}

gen_non_int_convertable() %as% {

  gen.element(
    list(
      integer(0)
    , double(0)
    , logical(0)
    , character(0)
    , NA_integer_
    , NA_real_
    , NA_character_
    , NA
    , NULL
    , NaN
    , -Inf
    , Inf
    , TRUE
    , FALSE
    , c(a = 1L)
    , c(a = 1)
    , factor(1L)
    , factor(1)
    , 1.1
    , "1"
    )
  )

}

gen_non_dbl_convertable() %as% {

  gen.element(
    list(
      integer(0)
    , double(0)
    , logical(0)
    , character(0)
    , NA_integer_
    , NA_real_
    , NA_character_
    , NA
    , NULL
    , NaN
    , -Inf
    , Inf
    , TRUE
    , FALSE
    , c(a = 1L)
    , c(a = 1)
    , factor(1L)
    , factor(1)
    , "1"
    )
  )

}

gen_non_chr_convertable() %as% {

  gen.element(
    list(
      integer(0)
    , double(0)
    , logical(0)
    , character(0)
    , NA_integer_
    , NA_real_
    , NA_character_
    , NA
    , NULL
    , NaN
    , -Inf
    , Inf
    , TRUE
    , FALSE
    , c(a = "a")
    , factor("a")
    , 1
    , 1L
    )
  )

}

gen_non_lgl_convertable() %as% {

  gen.element(
    list(
      integer(0)
    , double(0)
    , logical(0)
    , character(0)
    , NA_integer_
    , NA_real_
    , NA_character_
    , NA
    , NULL
    , NaN
    , -Inf
    , Inf
    , c(a = TRUE)
    , factor(TRUE)
    , "TRUE"
    , 1
    , 1L
    )
  )

}



gen_words(.min_len = 1, .max_len = 100) %as% {

  gen_chrs() |>
    gen.c(from = .min_len, to = .max_len) |>
    gen.and_then(\(x) paste0(x, collapse = ""))

}



gen_int_convertable_dbl_vecs(.min_len = 1, .max_len = 100) %as% {

  gen_int_convertable_dbls() |>
    gen.c(from = .min_len, to = .max_len)

}

