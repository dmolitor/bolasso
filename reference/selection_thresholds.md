# Calculate each covariate's smallest variable selection threshold

There are two methods of variable selection for covariates. The first is
the Variable Inclusion Probability (VIP) introduced by Bach (2008) and
generalized by Bunea et al (2011). The second is the Quantile confidence
interval (QNT) proposed by Abram et al (2016). For a given level of
significance alpha, each method selects covariates for the given
threshold = 1 - alpha. The higher the threshold (lower alpha), the more
stringent the variable selection criterion.

## Usage

``` r
selection_thresholds(object, grid = seq(0, 1, by = 0.01), ...)
```

## Arguments

- object:

  An object of class [bolasso](bolasso.md) or `bolasso_fast`.

- grid:

  A vector of numbers between 0 and 1 (inclusive) specifying the grid of
  threshold values to calculate variable inclusion criterion at.
  Defaults to `seq(0, 1, by = 0.01)`.

- ...:

  Additional parameters to pass to
  [`coef`](https://rdrr.io/r/stats/coef.html) on objects of class
  [bolasso](bolasso.md) and `bolasso_fast`.

## Value

A tibble with dimension (2*p)x5 where p is the number of covariates. In
the case of a multinomial regression, a list of tibbles with dimension
(2*p)x5 where the length of the list is N, where N is the number of
unique outcome values.

## Details

This function returns a tibble that, for each covariate, returns the
largest threshold (equivalently smallest alpha) at which it would be
selected for both the VIP and the QNT methods. Consequently the number
of rows in the returned tibble is 2\*p where p is the number of
covariates included in the model.
