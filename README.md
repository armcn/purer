
<!-- README.md is generated from README.Rmd. Please edit that file -->

# purer

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

The goal of purer is to build on the package lambda.r to create a more
strict type system for R and corresponding type safe functions. This
package depends on lambda.r and purer is designed to work within the
functional programming framework provided by lambda.r. Many functions
seen in the examples here use exported functions from lambda.r so the
documentation for that package should be read first:
<https://github.com/zatonovo/lambda.r>. purer is early in the
development phase.

## Installation

You can install the development version with:

``` r
remotes::install_github("AndrewMcNeilAnalytics/purer")
```

## Motivation

R is a dynamically typed language which performs implicit type
conversions liberally. This can be useful for interactive data analysis
but makes code written with R less predictable. purer is an attempt to
add some type safety to R code.

Here are some examples to illustrate the problem purer is trying to
solve.

#### The atomic vector types in R can have missing or other special values

These are useful in some circumstances but allow bugs to creep in.

``` r
is.integer(1L) #> TRUE
is.integer(NA_integer_) #> TRUE
is.integer(integer(0)) #> TRUE

is.double(1) #> TRUE
is.double(NA_real_) #> TRUE
is.double(NaN) #> TRUE
is.double(Inf) #> TRUE
is.double(double(0)) #> TRUE

is.character("a") #> TRUE
is.character(NA_character_) #> TRUE
is.character(character(0)) #> TRUE

is.logical(TRUE) #> TRUE
is.logical(T) #> TRUE
is.logical(NA) #> TRUE
is.logical(logical(0)) #> TRUE
```

#### R functions and operators convert types implicitly

``` r
1 + TRUE #> 2
1 + FALSE #> 1

1 == TRUE #> TRUE
1 == "1" #> TRUE

mean(c(TRUE, FALSE)) #> 0.5

paste("hello", 1) #> "hello 1"
```

#### R functions and operators accept missing or undefined values

``` r
1 + NA #> NA
#> [1] NA
1 + NULL #> numeric(0)
#> numeric(0)

mean(NA) #> NA
#> [1] NA

paste("hello", NA) #> "hello NA"
#> [1] "hello NA"
```

#### R functions and operators return missing or undefined values

``` r
1 + NA #> NA
#> [1] NA
1 + NULL #> numeric(0)
#> numeric(0)

1 == NULL #> logical(0)
#> logical(0)
1 & NA #> NA
#> [1] NA
```

#### Vectorized operations recycle vectors in an unpredictable way

``` r
x <- 1:3
y <- 1:10

x + y
#> Warning in x + y: longer object length is not a multiple of shorter object
#> length
#>  [1]  2  4  6  5  7  9  8 10 12 11
```

Having strict “pure” types is the first problem purer solves. “Pure”
refers to types that have values that you would expect to be there. No
missing values are allowed, as well as no undefined behavior such as
Inf, NA, or empty vectors.

## purer types

These types replace base integer, double, character, and logical.
Scalars are denoted with the type name and vectors with the type name
and trailing underscore. The type constructor function will throw an
error if the input vectors are not purer compatible.

``` r
library(purer)
#> Loading required package: lambda.r
```

``` r
int <- Int(1L) 
int_ <- Int_(c(1L, 2L)) 

int %isa% Int #> TRUE
int_ %isa% Int_ #> TRUE

dbl <- Dbl(1) 
dbl_ <- Dbl_(c(1, 2))

dbl %isa% Dbl #> TRUE
dbl_ %isa% Dbl_ #> TRUE

chr <- Chr("a") 
chr_ <- Chr_(c("a", "b")) 

chr %isa% Chr #> TRUE
chr_ %isa% Chr_ #> TRUE

lgl <- Lgl(TRUE) 
lgl_ <- Lgl_(c(TRUE, FALSE)) 

lgl %isa% Lgl #> TRUE
lgl_ %isa% Lgl_ #> TRUE

Int_(c(1L, NA_integer_)) #> error
```

These types work with any functions in R just as the base types would.
For example, they can be used with base arithmetic operators. This
improves code safety to some degree because you can be confident that
the input variables are “pure”, but the downside is that the output may
not preserve the purer types and the operators don’t check that the
inputs are valid types.

``` r
# good
Int(1L) + Int(1L) #> 2

# bad
Lgl(TRUE) + Lgl(TRUE) #> 2
```

## purer operators

To solve this problem purer introduces replacements for all base
operators, each with multiple syntax options. purer operators and
functions use type inference where the acceptable types are obvious. For
example, the add function can have two Int types or two Dbl types as
inputs. If the inputs are both Int then the output will be Int, but an
Int can’t be added to a Dbl. For this to happen one of the variable must
be explicitly converted to the other type. For binary operators the
.types argument denotes if the inputs are scalars or vectors.
Vectorization from R is preserved but it is made explicit.

``` r
x <- Int(1L)
y <- Int(2L)

x %+ss% y #> 3
# or
pure_add(x, y, .types = Chr("ss"))
# or 
add(x, y, Chr("ss"))

# Can be used easily with pipe
x |> add(y, Chr("ss"))

# Example of scalar to vector addition
x %+sv% Int_(c(1L, 2L)) #> Int_(c(2L, 3L))
```

#### Other operators

See the documentation for other operators but they all follow the same
conventions. The inputs must all be purer types and .types declares if
they are scalars or vectors. For example here is the greater than or
equal to operator.

``` r
x <- Int(1L)
y <- Int(2L)

x %>=ss% y #> Lgl(FALSE)

pure_gte(x, y, Chr("ss")) #> Lgl(FALSE)

x |> gte(y, Chr("ss")) #> Lgl(FALSE)
```

## purer functions

purer also introduces replacements for many R base and stats functions.
Here is an example of the mean function. The trailing underscore denotes
if the output should be a scalar or vector. It should only be a vector
in situations where the output is going to be implicitly combined back
into a vector, such as any operation in a group\_by context.

``` r
x <- Int_(1:10L)

pure_mean(x) #> Dbl(5.5)
pure_mean_(x) #> Dbl_(5.5)
```

If a function doesn’t have a purer replacement, safety can be increased
with any R function by following this pattern.

``` r
# Find the max of integer vector 
x <- 1:10L

# Regular way
max(x) #> 10

# purer way
x |> Int_() |> max() |> Int() #> Int(10)

# or define purer version of R function
my_pure_max(.x) %::% Int_ : Int
my_pure_max(.x) %as% {
  
  .x |> max() |> Int()
  
}

Int_(x) |> my_pure_max() #> Int(10)
```

## purer tibbles

Operations on vectors in R are commonly done in the context of tibbles
or data frames. purer introduces a simple wrapper around the tibble
class to make it easy to define tibbles with purer typed columns.

``` r
library(dplyr)
```

``` r
my_tbl <- tibble(
  a = c(1L, 2L),
  b = c(1, 2),
  c = c("a", "b"),
  d = c(TRUE, FALSE)
)

types <- list(
  a = "Int_",
  b = "Dbl_",
  c = "Chr_",
  d = "Lgl_"
)

my_purer_tbl <- Tbl(
  .x = my_tbl,
  .types = types
)

my_purer_tbl |> glimpse()
#> Rows: 2
#> Columns: 4
#> $ a <Int_> 1, 2
#> $ b <Dbl_> 1, 2
#> $ c <Chr_> "a", "b"
#> $ d <Lgl_> TRUE, FALSE
```

The intended use is to start and end every major data transformation
with a Tbl constructor. This acts as an assert statement for the inputs
and outputs of a transformation and also ensures all columns are purer
types. For example lets say I want to get the average miles/gallon for
each number of gears arranged in descending order by average
miles/gallon.

``` r
mtcars_subset <- mtcars |> 
  as_tibble() |> 
  select(gear, mpg)

# Regular way 
mtcars_subset |> 
  group_by(gear) |> 
  summarize(mean_mpg = mean(mpg)) |> 
  arrange(desc(mean_mpg)) |> 
  glimpse()
#> Rows: 3
#> Columns: 2
#> $ gear     <dbl> 4, 5, 3
#> $ mean_mpg <dbl> 24.53333, 21.38000, 16.10667

# purer way
input_types <- list(
  gear = "Int_",
  mpg = "Dbl_"
)

output_types <- list(
  gear = "Int_",
  mean_mpg = "Dbl_"
)
  
mtcars_subset |> 
  Tbl(.types = input_types) |> # input constructor/assert
  group_by(gear) |> 
  summarise(mean_mpg = pure_mean_(mpg)) |> 
  arrange(desc(mean_mpg)) |> 
  Tbl(.types = output_types) |> # output constructor/assert
  glimpse()
#> Rows: 3
#> Columns: 2
#> $ gear     <Int_> 4, 5, 3
#> $ mean_mpg <Dbl_> 24.53333, 21.38000, 16.10667
```

To take this further, the Tbl type can be extended to create custom
types for different datasets. Although it takes more work up front this
is the safest option and is recommended for production code.

``` r
# define input type
Mtcars(.x) %::% tbl_df : .
Mtcars(.x) %as% {
  
  types <- list(
    mpg = "Dbl_",
    cyl = "Int_",
    disp = "Dbl_",
    hp = "Int_",
    drat = "Dbl_",
    wt = "Dbl_",
    qsec = "Dbl_",
    vs = "Int_",
    am = "Int_",
    gear = "Int_",
    carb = "Int_"
  )
  
  Tbl(.x, types)
  
}

# define output type
MpgByGear(.x) %::% tbl_df : .
MpgByGear(.x) %as% {
  
  types <- list(
    mean_mpg = "Dbl_",
    gear = "Int_"
  )
  
  Tbl(.x, types)
  
}

# transformation function
transform_mtcars(.mtcars) %::% Mtcars : MpgByGear
transform_mtcars(.mtcars) %as% {
  
  .mtcars |> 
    group_by(gear) |> 
    summarise(mean_mpg = pure_mean_(mpg)) |> 
    arrange(desc(mean_mpg)) |> 
    MpgByGear()
  
}

# convert to Mtcars type
mtcars_pure <- mtcars |> 
  as_tibble() |> 
  Mtcars()

# transform Mtcars -> MpgByGear
mpg_by_gear <- mtcars_pure |> 
  transform_mtcars()

mpg_by_gear %isa% MpgByGear
#> [1] TRUE

mpg_by_gear |> glimpse()
#> Rows: 3
#> Columns: 2
#> $ gear     <Int_> 4, 5, 3
#> $ mean_mpg <Dbl_> 24.53333, 21.38000, 16.10667
```
