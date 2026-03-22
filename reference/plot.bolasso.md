# Plot a `bolasso` object

The method plots coefficient distributions for the covariates included
in the `bolasso` model. If there are more than 30 covariates included in
the full model, this will plot the 30 covariates with the largest
absolute mean coefficient. The user can also plot coefficient
distributions for a specified subset of covariates.

## Usage

``` r
# S3 method for class 'bolasso'
plot(x, covariates = NULL, ...)
```

## Arguments

- x:

  An object of class [bolasso](bolasso.md) or `bolasso_fast`.

- covariates:

  A subset of the covariates to plot. This should be a vector of
  covariate names either as strings or bare. E.g.
  `covariates = c("var_1", "var_2")` or `covariates = c(var_1, var_2)`.
  This argument is optional and is `NULL` by default. In this case it
  will plot up to 30 covariates with the largest absolute mean
  coefficients.

- ...:

  Additional arguments to pass directly to
  [`coef`](https://rdrr.io/r/stats/coef.html) for objects of class
  [bolasso](bolasso.md) or `bolasso_fast`.
