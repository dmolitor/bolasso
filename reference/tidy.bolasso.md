# Tidy a bolasso object

Tidy a bolasso object

## Usage

``` r
# S3 method for class 'bolasso'
tidy(x, select = c("lambda.min", "lambda.1se", "min", "1se"), ...)
```

## Arguments

- x:

  A `bolasso` object.

- select:

  One of "min", "1se", "lambda.min", "lambda.1se". Both "min" and
  "lambda.min" are equivalent and are the lambda value that minimizes cv
  MSE. Similarly "1se" and "lambda.1se" are equivalent and refer to the
  lambda that achieves the most regularization and is within 1se of the
  minimal cv MSE.

- ...:

  Additional arguments to pass directly to `coef.bolasso`.

## Value

A tidy
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
summarizing bootstrap-level coefficients for each covariate.
