
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

The goal of bolasso is to implement bootstrap-enhanced Lasso (and more
generally, penalized regression) estimation, as proposed originally in
[Bach (2008)](#2) and extended by [Bunea et al. (2011)](#3) and [Abram
et al. (2016)](#1). These methods focus primarily on variable selection
and propose two similar, but slightly different, variable selection
algorithms; the variable inclusion probability (VIP) algorithm (Bach;
Bunea et al.), and the bootstrap distribution quantile (QNT) algorithm
(Abram et al.). Beyond implementing both these variable selection
methods, bolasso also provides utilities for making bagged predictions,
examining coefficient distributions, and plotting.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("dmolitor/bolasso@dev")
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
library(ggplot2)
library(tibble)

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

First, let’s create a train/test split of our data, and then run
100-fold bootstrapped Lasso with `glmnet`.

``` r
train_idx <- sample(1:nrow(PimaIndiansDiabetes), round(0.7*nrow(PimaIndiansDiabetes)))
train <- PimaIndiansDiabetes[train_idx, ]
test <- PimaIndiansDiabetes[-train_idx, ]

model <- bolasso(
  diabetes ~ .,
  data = train,
  n.boot = 100,
  progress = FALSE,
  family = "binomial"
)
#> Loaded glmnet 4.1-8
```

### Variable selection

Next, using a threshold of 0.95 we can extract the selected variables
using the VIP method, which extracts all variables that were selected
(had non-zero coefficients) in \>= 95% of the bootstrapped models. We’ll
use the regularization parameter `lambda.min` that minimizes
cross-validation error.

``` r
selected_variables(model, threshold = 0.95, method = "vip", select = "lambda.min")
#> # A tibble: 100 × 5
#>    id     pregnant glucose   mass pedigree
#>    <chr>     <dbl>   <dbl>  <dbl>    <dbl>
#>  1 boot1    0.0957  0.0428 0.0811    0.845
#>  2 boot2    0       0.0367 0.0850    0.193
#>  3 boot3    0.0713  0.0258 0.0833    0.667
#>  4 boot4    0.0908  0.0349 0.0649    0.573
#>  5 boot5    0.217   0.0462 0.0680    0.813
#>  6 boot6    0.0930  0.0305 0.0814    0.372
#>  7 boot7    0.0516  0.0404 0.0875    1.18 
#>  8 boot8    0.0582  0.0327 0.0567    0.722
#>  9 boot9    0.0617  0.0360 0.104     0.353
#> 10 boot10   0.0389  0.0353 0.0721    0    
#> # ℹ 90 more rows
```

Note that this returned a tibble with the selected variables as columns
and the coefficients for each of the bootstrapped models as rows. If you
want to simply return only the variable names, you can add the
`var_names_only` argument:

``` r
selected_variables(model, 0.95, "vip", var_names_only = TRUE)
#> [1] "pregnant" "glucose"  "mass"     "pedigree"
```

We can compare the selected variables using the VIP method to the QNT
method, which selects all variables that have a 95% bootstrap confidence
interval that does not contain 0:

``` r
selected_variables(model, 0.95, "qnt", var_names_only = TRUE)
#> [1] "pregnant" "glucose"  "mass"     "pedigree"
```

Note that the number of selected variables with QNT will always be \<=
than with VIP. The default method for bolasso is `method = "vip"`.

#### Variable selection thresholds

It may be that, instead of selecting variables for a given threshold and
method, we want to see the largest threshold at which each variable
would be selected by both the VIP and QNT methods. We can quickly
visualize this with the `plot_selection_thresholds` function.

``` r
plot_selection_thresholds(model, select = "lambda.min")
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="75%" />

You can also get these thresholds in a tibble:

``` r
selection_thresholds(model, select = "lambda.min")
#> # A tibble: 16 × 5
#>    covariate method threshold  alpha covariate_id
#>    <chr>     <chr>      <dbl>  <dbl>        <int>
#>  1 age       QNT         0.77 0.23              8
#>  2 glucose   QNT         1    0                 2
#>  3 insulin   QNT         0.37 0.63              5
#>  4 mass      QNT         1    0                 6
#>  5 pedigree  QNT         0.95 0.0500            7
#>  6 pregnant  QNT         0.99 0.0100            1
#>  7 pressure  QNT         0.83 0.17              3
#>  8 triceps   QNT         0    1                 4
#>  9 age       VIP         0.9  0.1               8
#> 10 glucose   VIP         1    0                 2
#> 11 insulin   VIP         0.76 0.24              5
#> 12 mass      VIP         1    0                 6
#> 13 pedigree  VIP         0.97 0.0300            7
#> 14 pregnant  VIP         0.99 0.0100            1
#> 15 pressure  VIP         0.93 0.0700            3
#> 16 triceps   VIP         0.59 0.41              4
```

### Coefficients

#### All coefficients

bolasso also supports moving beyond variable selection and understanding
the bootstrapped variable coefficients. We can extract a tidy tibble
where each variable is a column, and each row represents a bootstrap
fold, and the values are the corresponding estimated coefficients.

``` r
tidy(model, select = "lambda.min")
#> # A tibble: 100 × 10
#>    id     Intercept pregnant glucose pressure  triceps   insulin   mass pedigree
#>    <chr>      <dbl>    <dbl>   <dbl>    <dbl>    <dbl>     <dbl>  <dbl>    <dbl>
#>  1 boot1      -9.91   0.0957  0.0428 -0.00212 -0.00254 -0.00429  0.0811    0.845
#>  2 boot2      -7.77   0       0.0367 -0.00859 -0.00789 -0.00100  0.0850    0.193
#>  3 boot3      -7.26   0.0713  0.0258 -0.00562  0       -0.000884 0.0833    0.667
#>  4 boot4      -7.92   0.0908  0.0349 -0.0115   0.0153   0        0.0649    0.573
#>  5 boot5      -8.48   0.217   0.0462 -0.0128  -0.00260 -0.00137  0.0680    0.813
#>  6 boot6      -8.20   0.0930  0.0305 -0.00585  0.0121  -0.00186  0.0814    0.372
#>  7 boot7     -10.2    0.0516  0.0404  0.00272 -0.00765 -0.00179  0.0875    1.18 
#>  8 boot8      -7.29   0.0582  0.0327 -0.00830  0       -0.00185  0.0567    0.722
#>  9 boot9      -8.96   0.0617  0.0360 -0.0201   0       -0.000922 0.104     0.353
#> 10 boot10     -8.23   0.0389  0.0353  0        0        0        0.0721    0    
#> # ℹ 90 more rows
#> # ℹ 1 more variable: age <dbl>
```

bolasso also allows us to plot the bootstrap distribution of variable
coefficients. Suppose that we want to quickly inspect this distribution
for each of our variables. We can achieve this by simply plotting our
model.

``` r
plot(model, select = "lambda.min")
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="75%" />

Now, suppose for example we are particularly interested in the
coefficient distributions for the `triceps`, `pressure`, and `glucose`
variables. We can plot the distributions for just these variables:

``` r
plot(model, covariates = c(glucose, pressure, triceps))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="75%" />

Note: If there are more than 30 variables included in our model, then
this will plot the 30 variables with the largest absolute mean
coefficients.

#### Selected variable coefficients

If we want to plot the coefficient distributions for only the selected
variables, we can use `plot_selected_variables` which will give us
pretty much the same thing as `plot`.

``` r
plot_selected_variables(
  model,
  threshold = 0.95,
  method = "vip",
  select = "lambda.min"
)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="75%" />

Just like `plot` we can also focus on a subset of our selected
variables.

``` r
plot_selected_variables(
  model,
  covariates = c(pregnant, mass),
  threshold = 0.95,
  method = "vip",
  select = "lambda.min"
)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="75%" />

### Predictions

Finally, we can make predictions using our bolasso model on new data.
For example, the following code shows how we would generate predicted
probabilites on our `test` data.

``` r
as_tibble(predict(model, test, select = "lambda.min", type = "response"))
#> # A tibble: 230 × 100
#>     boot1  boot2  boot3  boot4  boot5  boot6  boot7  boot8  boot9 boot10 boot11
#>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1 0.818  0.635  0.634  0.729  0.716  0.789  0.735  0.699  0.755  0.676  0.628 
#>  2 0.877  0.736  0.631  0.724  0.937  0.698  0.811  0.767  0.701  0.715  0.784 
#>  3 0.0276 0.0700 0.0704 0.0567 0.0338 0.0625 0.0292 0.0556 0.0380 0.0715 0.0682
#>  4 0.578  0.809  0.630  0.891  0.722  0.773  0.729  0.686  0.861  0.896  0.579 
#>  5 0.697  0.625  0.531  0.684  0.683  0.654  0.690  0.646  0.683  0.689  0.543 
#>  6 0.262  0.435  0.439  0.457  0.214  0.474  0.379  0.305  0.418  0.478  0.296 
#>  7 0.218  0.196  0.211  0.149  0.256  0.214  0.168  0.197  0.157  0.202  0.258 
#>  8 0.403  0.276  0.343  0.226  0.261  0.395  0.337  0.308  0.343  0.313  0.309 
#>  9 0.830  0.744  0.669  0.622  0.786  0.743  0.768  0.675  0.778  0.733  0.682 
#> 10 0.0296 0.0682 0.0695 0.0590 0.0427 0.0523 0.0382 0.0642 0.0342 0.0683 0.0710
#> # ℹ 220 more rows
#> # ℹ 89 more variables: boot12 <dbl>, boot13 <dbl>, boot14 <dbl>, boot15 <dbl>,
#> #   boot16 <dbl>, boot17 <dbl>, boot18 <dbl>, boot19 <dbl>, boot20 <dbl>,
#> #   boot21 <dbl>, boot22 <dbl>, boot23 <dbl>, boot24 <dbl>, boot25 <dbl>,
#> #   boot26 <dbl>, boot27 <dbl>, boot28 <dbl>, boot29 <dbl>, boot30 <dbl>,
#> #   boot31 <dbl>, boot32 <dbl>, boot33 <dbl>, boot34 <dbl>, boot35 <dbl>,
#> #   boot36 <dbl>, boot37 <dbl>, boot38 <dbl>, boot39 <dbl>, boot40 <dbl>, …
```

Note that this outputs an (n x p) matrix of predictions where n is the
number of rows in our test set, p is the number of bootstraps, and each
column represents the predictions from one of our bootstrapped models.
To combine these into a single prediction per observation, we could take
the average for each observation across the models:

``` r
tibble(
  predictions = rowMeans(
    predict(model, test, select = "lambda.min", type = "response")
  )
)
#> # A tibble: 230 × 1
#>    predictions
#>          <dbl>
#>  1      0.696 
#>  2      0.744 
#>  3      0.0533
#>  4      0.747 
#>  5      0.623 
#>  6      0.397 
#>  7      0.196 
#>  8      0.309 
#>  9      0.722 
#> 10      0.0536
#> # ℹ 220 more rows
```

### Fast estimation 🏎️💨

For each bootstrapped model, bolasso uses cross-validation to find the
optimal regularization parameter lambda. In glmnet, the default number
of cross-validation folds is 10. This can quickly become computationally
expensive and slow, especially when using many bootstrap replicates. For
example, with 1,000 bootstrap replicates, this results in estimating
models on 10,000 cross-validation sets.

To address this, we can activate the `fast = TRUE` argument in bolasso.
Instead of using cross-validation to find the optimal lambda for each
bootstrap model, fast bolasso runs a single cross-validated regression
on the full dataset to identify the optimal lambda. Then each
bootstrapped model uses that lambda as its regularization parameter.

The following comparison shows the computation time of standard bolasso
vs fast bolasso across increasing bootstrap replicates. The plot
displays the number of seconds each algorithm takes to complete.

``` r
# Compare standard vs. fast bolasso across different bootstrap values
times <- lapply(
  c(10, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1e3),
  \(x) {
    time_standard <- system.time({
      bolasso(
        diabetes ~ .,
        data = train,
        n.boot = x,
        progress = FALSE,
        family = "binomial"
      )
    })
    time_fast <- system.time({
      bolasso(
        diabetes ~ .,
        data = train,
        n.boot = x,
        progress = FALSE,
        family = "binomial",
        fast = TRUE
      )
    })
    return(
      tibble::tibble("regular" = time_standard[[3]], "fast" = time_fast[[3]])
    )
  }
)

# Make a data.frame out of the times
times_df <- do.call(rbind, times) |>
  transform(
    id = 1:11,
    n_bootstrap = c(10, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1e3)
  ) |>
  reshape(
    varying = c("regular", "fast"),
    v.names = "time",
    times = c("regular", "fast"),
    timevar = "algorithm",
    idvar = c("id", "n_bootstrap"),
    direction = "long"
  )

# Plot it!
ggplot(times_df, aes(x = n_bootstrap, y = time, color = factor(algorithm))) +
  geom_point() +
  geom_line() +
  labs(x = "N Bootstraps", y = "Time (seconds)") +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  theme_minimal() +
  theme(legend.title = element_blank())
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="75%" />

Fast bolasso clearly achieves some pretty massive speedups over the
standard version! This difference in speed will only be more accentuated
when estimating on larger datasets.

#### What do we lose with standard vs. fast?

There’s never a free lunch, so to be clear about the tradeoffs between
the standard and fast versions of bolasso, the following shows the
difference in predictive accuracy on our hold-out test set.

``` r
model_standard <- bolasso(
  diabetes ~ .,
  data = train,
  n.boot = 100,
  progress = FALSE,
  family = "binomial"
)
model_fast <- bolasso(
  diabetes ~ .,
  data = train,
  n.boot = 100,
  progress = FALSE,
  family = "binomial",
  fast = TRUE
)

model_standard_preds <- ifelse(
  rowMeans(predict(model_standard, test, type = "response")) >= 0.5,
  yes = 1,
  no = 0
)
model_fast_preds <- ifelse(
  rowMeans(predict(model_fast, test, type = "response")) >= 0.5,
  yes = 1,
  no = 0
)
truth <- as.integer(test$diabetes) - 1
```

    #> Standard Bolasso accuracy: 77.39 %
    #>  Fast Bolasso accuracy: 78.7 %

It’s important to note that fast bolasso should be thought of more as a
rough-and-ready algorithm that is better for quick iteration and might
have worse empirical performance than the standard algorithm.

#### Parallelizing bolasso

We can also fit bolasso bootstrap models in parallel via the
[future](https://CRAN.R-project.org/package=future) package. The future
package supports a wide variety of parallelization, from local
multi-core to remote compute clusters. Parallelizing bolasso is as
simple as initializing the parallel method prior to executing the
bolasso function. For example, the following setup will execute bolasso
in parallel R sessions.

``` r
future::plan("multisession")
time_parallel <- system.time({
  bolasso(
    diabetes ~ .,
    data = train,
    n.boot = 1000,
    progress = FALSE,
    family = "binomial"
  )
})
future::plan("sequential")

time_sequential <- system.time({
  bolasso(
    diabetes ~ .,
    data = train,
    n.boot = 1000,
    progress = FALSE,
    family = "binomial"
  )
})
```

    #> Parallel bolasso time (seconds): 10.827 
    #> Sequential bolasso time (seconds): 41.749

### Beyond the Lasso

bolasso also allows us to fit penalized regression models beyond the
Lasso. For example, suppose we want to fit a bootstrap-enhanced
elasticnet model with a mixing parameter of 0.5 (an even mix of the
Ridge and Lasso regularization terms). We can simply pass the underlying
`glmnet::glmnet` argument `alpha = 0.5` through bolasso. The following
code compares selected variables between the Lasso and elasticnet
models.

``` r
lasso <- bolasso(
  diabetes ~ .,
  data = train,
  n.boot = 100,
  progress = FALSE,
  family = "binomial"
)

elnet <- bolasso(
  diabetes ~ .,
  data = train,
  n.boot = 100,
  progress = FALSE,
  family = "binomial",
  alpha = 0.5
)
```

    #> Lasso selected variables: pregnant glucose mass 
    #> Elnet selected variables: pregnant glucose pressure mass pedigree age

## References

<a id="1">\[1\]</a>Abram, Samantha V et al. “Bootstrap Enhanced
Penalized Regression for Variable Selection with Neuroimaging Data.”
Frontiers in neuroscience vol. 10 344. 28 Jul. 2016,
<doi:10.3389/fnins.2016.00344>

<a id="2">\[2\]</a>Bach, Francis. “Bolasso: Model Consistent Lasso
Estimation through the Bootstrap.” ArXiv:0804.1302 \[Cs, Math, Stat\],
2008. <https://arxiv.org/abs/0804.1302>.

<a id="3">\[3\]</a>Bunea, Florentina et al. “Penalized least squares
regression methods and applications to neuroimaging.” NeuroImage
vol. 55,4 (2011): 1519-27. <doi:10.1016/j.neuroimage.2010.12.028>
