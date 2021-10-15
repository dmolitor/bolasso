
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bolasso <a href='https://dmolitor.github.io/bolasso/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dmolitor/bolasso/workflows/R-CMD-check/badge.svg)](https://github.com/dmolitor/bolasso/actions)
<!-- badges: end -->

The goal of bolasso is to implement model-consistent Lasso estimation
via the bootstrap [\[1\]](#1).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmolitor/bolasso")
```

## Variable selection with bolasso

To illustrate the usage of bolasso, we’ll use the [Pima Indians Diabetes
dataset](https://github.com/jbrownlee/Datasets/blob/master/pima-indians-diabetes.names)
to determine which factors are important predictors of testing positive
for diabetes. For a full description of the input variables, see the
link above.

### Load requisite packages and data

``` r
library(bolasso)
library(readr)

diabetes <- read_csv(
  "https://raw.githubusercontent.com/jbrownlee/Datasets/master/pima-indians-diabetes.csv",
  col_names = c(paste0("V", 1:8), "outcome")
)
```

First, we run 100-fold bootstrapped Lasso with the `glmnet`
implementation. We can get a rough estimate of the elapsed time using
`Sys.time()`.

``` r
start.time <- Sys.time()

model <- bolasso(
  outcome ~ .,
  data = diabetes,
  n.boot = 100, 
  implement = "glmnet",
  family = "binomial"
)

Sys.time() - start.time
#> Time difference of 29.13701 secs
```

We can get a quick overview of the model by printing the `bolasso`
object.

``` r
model
#> ------------- 100-fold bootstrapped Lasso -------------
#> 
#> Model matrix dimensions:
#>    - 8 Predictors
#>    - 768 Observations
#> 
#> Selected variables:
#>    - 6/8 predictors selected with 90% threshold
#>    - 4/8 predictors selected with 100% threshold
```

### Extracting selected variables

Next, we can extract all variables that were selected in 90% and 100% of
the bootstrapped Lasso models. We can also pass any relevant arguments
to `predict` on the `cv.glmnet` or `cv.gamlr` model objects. In this
case we will use the lambda value that minimizes OOS error.

``` r
selected_vars(model,
              threshold = 0.9,
              select = "lambda.min")
#> # A tibble: 7 x 2
#>   variable  mean_coef
#>   <chr>         <dbl>
#> 1 Intercept   -8.20  
#> 2 V1           0.120 
#> 3 V2           0.0347
#> 4 V3          -0.0118
#> 5 V6           0.0866
#> 6 V7           0.849 
#> 7 V8           0.0139
selected_vars(model,
              threshold = 1,
              select = "lambda.min")
#> # A tibble: 5 x 2
#>   variable  mean_coef
#>   <chr>         <dbl>
#> 1 Intercept   -8.20  
#> 2 V1           0.120 
#> 3 V2           0.0347
#> 4 V3          -0.0118
#> 5 V6           0.0866
```

### Plotting selected variables

We can also quickly plot the selected variables at the 90% and 100%
threshold values.

``` r
plot(model, threshold = 0.9)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="75%" />

``` r
plot(model, threshold = 1)
```

<img src="man/figures/README-unnamed-chunk-6-2.png" width="75%" />

### Parallelizing bolasso

We can execute `bolasso` in parallel via the
[future](https://cran.r-project.org/web/packages/future/index.html)
package. To do so we can copy the code from above with only one minor
tweak shown below.

``` r
future::plan("multisession")
```

We can now run the code from above, unaltered, and it will execute in
parallel.

``` r
start.time <- Sys.time()

model <- bolasso(
  outcome ~ .,
  data = diabetes,
  n.boot = 100, 
  implement = "glmnet",
  family = "binomial"
)

Sys.time() - start.time
#> Time difference of 13.5281 secs
```

## References

<a id="1">\[1\]</a> Bach, Francis. “Bolasso: Model Consistent Lasso
Estimation through the Bootstrap.” ArXiv:0804.1302 \[Cs, Math, Stat\],
April 8, 2008. <http://arxiv.org/abs/0804.1302>.
