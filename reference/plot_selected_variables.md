# Plot selected variables from a `bolasso` object.

The method plots coefficient distributions for the selected covariates
in the `bolasso` model. If there are more than 30 selected covariates,
this will plot the 30 selected covariates with the largest absolute mean
coefficient. The user can also plot coefficient distributions for a
specified subset of selected covariates.

## Usage

``` r
plot_selected_variables(
  x,
  covariates = NULL,
  threshold = 0.95,
  method = c("vip", "qnt"),
  ...
)
```

## Arguments

- x:

  An object of class [bolasso](bolasso.md) or `bolasso_fast`.

- covariates:

  A subset of the selected covariates to plot. This should be a vector
  of covariate names either as strings or bare. E.g.
  `covariates = c("var_1", "var_2")` or `covariates = c(var_1, var_2)`.
  This argument is optional and is `NULL` by default. In this case it
  will plot up to 30 covariates with the largest absolute mean
  coefficients.

- threshold:

  A numeric between 0 and 1, specifying the variable selection threshold
  to use.

- method:

  The variable selection method to use. The two valid options are
  `c("vip", "qnt")`. The default `"vip"` and is the method described in
  the original Bach (2008) and complementary Bunea et al. (2011) works.
  The `"qnt"` method is the method proposed by Abram et al. (2016).

- ...:

  Additional arguments to pass to
  [`coef`](https://rdrr.io/r/stats/coef.html) on objects with class
  `bolasso` or `bolass_fast`.
