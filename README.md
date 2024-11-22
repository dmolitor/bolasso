
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
et al.](#1). These methods focus primarily on variable selection and
propose two similar, but slightly different, variable selection
algorithms; the variable inclusion probability (VIP) algorithm (Bach,
2008; Bunea et al., 2011), and the bootstrap distribution quantile (QNT)
algorithm (Abram et al., 2016). Beyond implementing both these variable
selection methods, bolasso also provides utilities for making bagged
predictions, examining coefficient distributions, and plotting.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("tidyverse/tibble")
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
  data = PimaIndiansDiabetes,
  n.boot = 100,
  progress = FALSE,
  family = "binomial"
)
#> Loaded glmnet 4.1-8
```

### Variable selection

Next, using a threshold of 0.95 we can extract the selected variables
using the VIP method ([Bach](#2) and [Bunea et al.](#3)), which selects
all variables that were selected (non-zero coefficient) in \>= 95% of
the bootstrapped models. We’ll use the regularization parameter lambda
that minimizes cross-validation error.

``` r
selected_variables(model, threshold = 0.95, method = "vip", select = "lambda.min")
#> # A tibble: 100 × 6
#>    id     pregnant glucose pressure   mass pedigree
#>    <chr>     <dbl>   <dbl>    <dbl>  <dbl>    <dbl>
#>  1 boot1    0.0997  0.0341 -0.0182  0.0744    0.629
#>  2 boot2    0.118   0.0406 -0.0133  0.0659    1.45 
#>  3 boot3    0.127   0.0354 -0.00767 0.0894    0.534
#>  4 boot4    0.172   0.0349 -0.0118  0.0658    0.950
#>  5 boot5    0.123   0.0330 -0.0121  0.0760    0.667
#>  6 boot6    0.152   0.0342 -0.0112  0.0777    0.828
#>  7 boot7    0.0689  0.0374 -0.0119  0.0874    0.748
#>  8 boot8    0.145   0.0338 -0.0124  0.0881    0.977
#>  9 boot9    0.155   0.0331 -0.00740 0.0623    1.52 
#> 10 boot10   0.144   0.0403 -0.00748 0.0519    0.651
#> # ℹ 90 more rows
```

Note that this returned a tibble with the selected variables as columns
and the coefficients for each of the bootstrapped models as rows. If you
want to simply return only the variable names, you can add the
`var_names_only` argument:

``` r
selected_variables(model, 0.95, "vip", var_names_only = TRUE)
#> [1] "pregnant" "glucose"  "pressure" "mass"     "pedigree"
```

We can compare the selected variables using the VIP method to the QNT
method ([Abram et al., 2016](#1)), which selects all variables that have
a 95% bootstrap confidence interval that does not contain 0:

``` r
selected_variables(model, 0.95, "qnt", var_names_only = TRUE)
#> [1] "pregnant" "glucose"  "pressure" "mass"     "pedigree"
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
#>  1 age       QNT         0.73 0.27              8
#>  2 glucose   QNT         1    0                 2
#>  3 insulin   QNT         0.55 0.45              5
#>  4 mass      QNT         1    0                 6
#>  5 pedigree  QNT         1    0                 7
#>  6 pregnant  QNT         1    0                 1
#>  7 pressure  QNT         0.95 0.0500            3
#>  8 triceps   QNT         0    1                 4
#>  9 age       VIP         0.86 0.14              8
#> 10 glucose   VIP         1    0                 2
#> 11 insulin   VIP         0.82 0.18              5
#> 12 mass      VIP         1    0                 6
#> 13 pedigree  VIP         1    0                 7
#> 14 pregnant  VIP         1    0                 1
#> 15 pressure  VIP         0.97 0.0300            3
#> 16 triceps   VIP         0.67 0.33              4
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
#>    id     Intercept pregnant glucose pressure   triceps  insulin   mass pedigree
#>    <chr>      <dbl>    <dbl>   <dbl>    <dbl>     <dbl>    <dbl>  <dbl>    <dbl>
#>  1 boot1      -7.61   0.0997  0.0341 -0.0182   0.00410  -2.66e-3 0.0744    0.629
#>  2 boot2      -9.01   0.118   0.0406 -0.0133   0.0140   -2.44e-3 0.0659    1.45 
#>  3 boot3      -8.67   0.127   0.0354 -0.00767 -0.00250  -9.96e-4 0.0894    0.534
#>  4 boot4      -7.29   0.172   0.0349 -0.0118  -0.00461  -3.65e-4 0.0658    0.950
#>  5 boot5      -7.78   0.123   0.0330 -0.0121  -0.00291  -6.54e-4 0.0760    0.667
#>  6 boot6      -7.73   0.152   0.0342 -0.0112   0.00558  -1.58e-3 0.0777    0.828
#>  7 boot7      -7.99   0.0689  0.0374 -0.0119  -0.000679 -1.36e-3 0.0874    0.748
#>  8 boot8      -8.14   0.145   0.0338 -0.0124  -0.00252  -3.05e-4 0.0881    0.977
#>  9 boot9      -7.69   0.155   0.0331 -0.00740 -0.00191  -8.19e-4 0.0623    1.52 
#> 10 boot10     -7.60   0.144   0.0403 -0.00748  0        -8.98e-4 0.0519    0.651
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
variables, we can use the `plot_selected_variables` which will give us
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
  covariates = c(pregnant, pressure),
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
#>  1 0.772  0.823  0.663  0.700  0.701  0.682  0.662  0.682  0.643  0.697  0.769 
#>  2 0.794  0.855  0.765  0.879  0.792  0.801  0.786  0.803  0.834  0.890  0.728 
#>  3 0.0464 0.0300 0.0400 0.0541 0.0486 0.0487 0.0539 0.0445 0.0394 0.0528 0.0479
#>  4 0.665  0.746  0.704  0.733  0.755  0.604  0.710  0.725  0.559  0.785  0.851 
#>  5 0.652  0.721  0.586  0.682  0.658  0.555  0.584  0.622  0.592  0.696  0.703 
#>  6 0.294  0.334  0.333  0.303  0.328  0.327  0.403  0.370  0.271  0.279  0.421 
#>  7 0.207  0.145  0.201  0.279  0.225  0.210  0.183  0.215  0.201  0.243  0.169 
#>  8 0.348  0.252  0.316  0.352  0.357  0.270  0.238  0.324  0.269  0.269  0.337 
#>  9 0.744  0.680  0.741  0.752  0.740  0.687  0.707  0.732  0.633  0.725  0.781 
#> 10 0.0457 0.0388 0.0401 0.0707 0.0544 0.0506 0.0569 0.0522 0.0590 0.0659 0.0428
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
#>  1      0.701 
#>  2      0.790 
#>  3      0.0479
#>  4      0.724 
#>  5      0.631 
#>  6      0.371 
#>  7      0.205 
#>  8      0.311 
#>  9      0.722 
#> 10      0.0532
#> # ℹ 220 more rows
```

### Fast estimation 🏎️💨

For each bootstrapped model, bolasso uses cross-validation to find the
optimal regularization parameter lambda. In glmnet, the default number
of cross-validation folds is 10. This can quickly become computationally
expensive and slow, especially when using many bootstrap replicates. For
example, with 1,000 bootstrap replicates, this results in estimating
models on 10,000 cross-validation sets.

To address this, we can activate the `fast = TRUE` flag in bolasso.
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

cat(
  "Standard Bolasso accuracy:",
  round(100*sum(model_standard_preds == truth)/length(truth), 2),
  "%\n",
  "\rFast Bolasso accuracy:",
  round(100*sum(model_fast_preds == truth)/length(truth), 2),
  "%\n"
)
#> Standard Bolasso accuracy: 77.39 %
#>  Fast Bolasso accuracy: 76.52 %
```

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
    progress = TRUE,
    family = "binomial"
  )
})

cat(
  "Parallel bolasso time (seconds):",
  round(time_parallel[[3]], 3),
  "\nSequential bolasso time (seconds):",
  round(time_sequential[[3]], 3)
)
#> Parallel bolasso time (seconds): 10.547 
#> Sequential bolasso time (seconds): 41.705
```

## References

<a id="1">\[3\]</a>Abram, Samantha V et al. “Bootstrap Enhanced
Penalized Regression for Variable Selection with Neuroimaging Data.”
Frontiers in neuroscience vol. 10 344. 28 Jul. 2016,
<doi:10.3389/fnins.2016.00344>

<a id="2">\[1\]</a>Bach, Francis. “Bolasso: Model Consistent Lasso
Estimation through the Bootstrap.” ArXiv:0804.1302 \[Cs, Math, Stat\],
2008. <https://arxiv.org/abs/0804.1302>.

<a id="3">\[2\]</a>Bunea, Florentina et al. “Penalized least squares
regression methods and applications to neuroimaging.” NeuroImage
vol. 55,4 (2011): 1519-27. <doi:10.1016/j.neuroimage.2010.12.028>
