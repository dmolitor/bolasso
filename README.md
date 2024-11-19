<!-- README.md is generated from README.Rmd. Please edit that file -->

# bolasso <a href='https://www.dmolitor.com/bolasso/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/dmolitor/bolasso/workflows/R-CMD-check/badge.svg)](https://github.com/dmolitor/bolasso/actions)
[![pkgdown](https://github.com/dmolitor/bolasso/workflows/pkgdown/badge.svg)](https://github.com/dmolitor/bolasso/actions)
[![Codecov test
coverage](https://codecov.io/gh/dmolitor/bolasso/branch/main/graph/badge.svg)](https://app.codecov.io/gh/dmolitor/bolasso?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/bolasso)](https://CRAN.R-project.org/package=bolasso)
<!-- badges: end -->

The goal of bolasso is to implement model-consistent Lasso estimation
via the bootstrap [\[1\]](#1).

## Installation

You can install from [CRAN](https://cran.r-project.org/web/packages/bolasso/index.html) with:
```r
install.packages("bolasso")
```

or install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dmolitor/bolasso")
```

## Usage

To illustrate the usage of bolasso, we’ll use the [Pima Indians Diabetes
dataset](http://math.furman.edu/~dcs/courses/math47/R/library/mlbench/html/PimaIndiansDiabetes.html)
to determine which factors are important predictors of testing positive
for diabetes. For a full description of the input variables, see the
link above.

### Load requisite packages and data

``` r
library(bolasso)

data(PimaIndiansDiabetes, package = "mlbench")

# Quick overview of the dataset
str(PimaIndiansDiabetes)
#> 'data.frame':    768 obs. of  9 variables:
#>  $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
#>  $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
#>  $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
#>  $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
#>  $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
#>  $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
#>  $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
#>  $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
#>  $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...
```

First, we run 100-fold bootstrapped Lasso with the `glmnet`
implementation. We can get a rough estimate of the elapsed time using
`system.time()`.

``` r
system.time({
  model <- bolasso(
    diabetes ~ .,
    data = PimaIndiansDiabetes,
    n.boot = 100, 
    implement = "glmnet",
    family = "binomial"
  )
})
#> Loaded glmnet 4.1-3
#>    user  system elapsed 
#>   19.32    0.14   19.58
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
selected_vars(model, threshold = 0.9, select = "lambda.min")
#> # A tibble: 7 x 2
#>   variable  mean_coef
#>   <chr>         <dbl>
#> 1 Intercept   -8.15  
#> 2 pregnant     0.119 
#> 3 glucose      0.0348
#> 4 pressure    -0.0113
#> 5 mass         0.0821
#> 6 pedigree     0.849 
#> 7 age          0.0138

selected_vars(model, threshold = 1, select = "lambda.min")
#> # A tibble: 5 x 2
#>   variable  mean_coef
#>   <chr>         <dbl>
#> 1 Intercept   -8.15  
#> 2 pregnant     0.119 
#> 3 glucose      0.0348
#> 4 mass         0.0821
#> 5 pedigree     0.849
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
[future](https://CRAN.R-project.org/package=future) package. To do so we
can copy the code from above with only one minor tweak shown below.

``` r
future::plan("multisession")
```

We can now run the code from above, unaltered, and it will execute in
parallel.

``` r
system.time({
  model <- bolasso(
    diabetes ~ .,
    data = PimaIndiansDiabetes,
    n.boot = 100, 
    implement = "glmnet",
    family = "binomial"
  )
})
#>    user  system elapsed 
#>    0.17    0.03    5.58
```

## References

<a id="1">\[1\]</a> Bach, Francis. “Bolasso: Model Consistent Lasso
Estimation through the Bootstrap.” ArXiv:0804.1302 \[Cs, Math, Stat\],
April 8, 2008. <https://arxiv.org/abs/0804.1302>.
