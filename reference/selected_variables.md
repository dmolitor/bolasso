# Bolasso-selected Variables

Identifies covariates that are selected by the Bolasso algorithm at the
user-defined threshold. There are two variable selection criterion to
choose between; Variable Inclusion Probability ("vip") introduced in the
original Bolasso paper (Bach, 2008) and further developed by Bunea et
al. (2011), and the Quantile ("qnt") approach proposed by Abram et al.
(2016). The desired threshold value is 1 - alpha, where alpha is some
(typically small) significance level.

## Usage

``` r
selected_variables(
  object,
  threshold = 0.95,
  method = c("vip", "qnt"),
  var_names_only = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class [bolasso](bolasso.md).

- threshold:

  A numeric between 0 and 1, specifying the variable selection threshold
  to use.

- method:

  The variable selection method to use. The two valid options are
  `c("vip", "qnt")`. The default `"vip"` and is the method described in
  the original Bach (2008) and complementary Bunea et al. (2011) works.
  The `"qnt"` method is the method proposed by Abram et al. (2016).

- var_names_only:

  A boolean value. When `var_names_only = FALSE` (the default value)
  this function will return a
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  of selected covariates and their corresponding coefficients across all
  bootstrap replicates. When `var_names_only == TRUE`, it will return a
  vector containing all selected covariate names.

- ...:

  Additional arguments to pass to
  [`coef`](https://rdrr.io/r/stats/coef.html) on objects with class
  [bolasso](bolasso.md) or `bolass_fast`.

## Value

A tibble with each selected variable and its respective coefficient for
each bootstrap replicate OR a vector of the names of all selected
variables. In the case of a multinomial regression this will return a
list with one element for each unique outcome. Each element will then be
either a tibble of coefficients or a list of names.

## Details

This function returns either a
[tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html) of
selected covariates and their corresponding coefficients across all
bootstrap replicates, or a vector of selected covariate names.

## See also

[`glmnet::coef.glmnet()`](https://glmnet.stanford.edu/reference/predict.glmnet.html)
and `gamlr:::coef.gamlr` for details on additional arguments to pass to
`...`.
