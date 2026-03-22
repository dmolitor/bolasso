# Bootsrap-enhanced Lasso

This function implements model-consistent Lasso estimation through the
bootstrap. It supports parallel processing by way of the
[future](https://CRAN.R-project.org/package=future) package, allowing
the user to flexibly specify many parallelization methods. This method
was developed as a variable-selection algorithm, but this package also
supports making ensemble predictions on new data using the bagged Lasso
models.

## Usage

``` r
bolasso(
  formula,
  data,
  n.boot = 100,
  progress = TRUE,
  implement = c("glmnet", "gamlr"),
  x = NULL,
  y = NULL,
  fast = FALSE,
  ...
)
```

## Arguments

- formula:

  An optional object of class
  [formula](https://rdrr.io/r/stats/formula.html) (or one that can be
  coerced to that class): a symbolic description of the model to be
  fitted. Can be omitted when `x` and `y` are non-missing.

- data:

  An optional object of class
  [data.frame](https://rdrr.io/r/base/data.frame.html) that contains the
  modeling variables referenced in `form`. Can be omitted when `x` and
  `y` are non-missing.

- n.boot:

  An integer specifying the number of bootstrap replicates.

- progress:

  A boolean indicating whether to display progress across bootstrap
  folds.

- implement:

  A character; either 'glmnet' or 'gamlr', specifying which Lasso
  implementation to utilize. For specific modeling details, see
  [`glmnet::cv.glmnet`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  or [`gamlr::cv.gamlr`](https://rdrr.io/pkg/gamlr/man/cv.gamlr.html).

- x:

  An optional predictor matrix in lieu of `form` and `data`.

- y:

  An optional response vector in lieu of `form` and `data`.

- fast:

  A boolean. Whether or not to fit a "fast" bootstrap procedure. If
  `fast == TRUE`, `bolasso` will fit
  [glmnet::cv.glmnet](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  on the entire dataset. It will then fit all bootstrapped models with
  the value of lambda (regularization parameter) that minimized
  cross-validation loss in the full model. If `fast == FALSE` (the
  default), `bolasso` will use cross-validation to find the optimal
  lambda for each bootstrap model.

- ...:

  Additional parameters to pass to either
  [`glmnet::cv.glmnet`](https://glmnet.stanford.edu/reference/cv.glmnet.html)
  or [`gamlr::cv.gamlr`](https://rdrr.io/pkg/gamlr/man/cv.gamlr.html).

## Value

An object of class `bolasso`. This object is a list of length `n.boot`
of `cv.glmnet` or `cv.gamlr` objects.

## See also

[glmnet::cv.glmnet](https://glmnet.stanford.edu/reference/cv.glmnet.html)
and [gamlr::cv.gamlr](https://rdrr.io/pkg/gamlr/man/cv.gamlr.html) for
full details on the respective implementations and arguments that can be
passed to `...`.

## Examples

``` r
mtcars[, c(2, 10:11)] <- lapply(mtcars[, c(2, 10:11)], as.factor)
idx <- sample(nrow(mtcars), 22)
mtcars_train <- mtcars[idx, ]
mtcars_test <- mtcars[-idx, ]

## Formula Interface

# Train model
set.seed(123)
bolasso_form <- bolasso(
  form = mpg ~ .,
  data = mtcars_train,
  n.boot = 20,
  nfolds = 5
)

# Retrieve a tidy tibble of bootstrap coefficients for each covariate
tidy(bolasso_form)
#> # A tibble: 20 × 19
#>    id    Intercept  cyl4   cyl6     cyl8     disp       hp    drat     wt   qsec
#>    <chr>     <dbl> <dbl>  <dbl>    <dbl>    <dbl>    <dbl>   <dbl>  <dbl>  <dbl>
#>  1 boot1     33.2  0     -1.49   0        0       -0.0379  0       -2.43   0    
#>  2 boot2     31.8  2.48   0     -1.50e+0  0       -0.00705 0       -3.70   0    
#>  3 boot3     22.0  5.55   0      0        0       -0.0134  0       -0.911  0    
#>  4 boot4     27.0  0     -1.22   9.70e-1  0       -0.0840  1.08e+0  0      0    
#>  5 boot5     22.7  0      0      0       -0.00680 -0.0365  1.65e+0 -0.654  0    
#>  6 boot6     31.0  1.52   0      0        0       -0.0201  0       -2.96   0    
#>  7 boot7     14.3  1.86   0     -1.26e+0  0       -0.00539 7.41e+0  0     -0.780
#>  8 boot8     17.7  1.93   0      0        0        0       9.43e-1 -2.24   0.393
#>  9 boot9     36.1  0.998 -0.109  0        0       -0.0295  0       -3.63   0    
#> 10 boot…     28.3  3.42   0      0        0        0       0       -3.13   0    
#> 11 boot…     22.0  4.41   0      0        0        0       2.82e+0 -0.617 -0.690
#> 12 boot…      7.85 0.894  0     -2.71e+0  0        0       3.54e+0  0      0    
#> 13 boot…      9.33 2.58   0     -9.25e-1  0        0       0       -0.701  0.401
#> 14 boot…      8.65 0      0     -5.72e-1  0        0       2.59e+0  0      0    
#> 15 boot…     23.0  2.40   0     -3.00e-4  0       -0.0355  0        0      0    
#> 16 boot…     24.3  0.460  0     -3.01e-1 -0.0107   0       1.96e-4 -1.15   0    
#> 17 boot…     22.2  2.68   0     -1.76e+0 -0.00510  0       0       -1.05   0    
#> 18 boot…     33.7  1.08   0      0        0       -0.0256  0       -3.20   0    
#> 19 boot…     30.8  1.84   0      0        0       -0.0244  0       -2.78   0    
#> 20 boot…     25.9  0      0     -2.67e+0 -0.00332 -0.00989 4.77e-1 -1.69   0    
#> # ℹ 9 more variables: vs <dbl>, am <dbl>, gear4 <dbl>, gear5 <dbl>,
#> #   carb2 <dbl>, carb3 <dbl>, carb4 <dbl>, carb6 <dbl>, carb8 <dbl>

# Extract selected variables
selected_variables(bolasso_form, threshold = 0.9, select = "lambda.min")
#> # A tibble: 20 × 1
#>    id    
#>    <chr> 
#>  1 boot1 
#>  2 boot2 
#>  3 boot3 
#>  4 boot4 
#>  5 boot5 
#>  6 boot6 
#>  7 boot7 
#>  8 boot8 
#>  9 boot9 
#> 10 boot10
#> 11 boot11
#> 12 boot12
#> 13 boot13
#> 14 boot14
#> 15 boot15
#> 16 boot16
#> 17 boot17
#> 18 boot18
#> 19 boot19
#> 20 boot20

# Bagged ensemble prediction on test data
predict(bolasso_form,
        new.data = mtcars_test,
        select = "lambda.min")
#>                     boot1    boot2    boot3     boot4    boot5    boot6
#> Mazda RX4        23.53203 21.31155 20.38801 22.290720 22.27351 21.94408
#> Datsun 710       26.39838 25.65987 30.73719 27.331865 23.36205 24.69222
#> Duster 360       15.27134 15.34670 13.64426  8.423965 14.22472 15.46800
#> Merc 240D        23.13465 22.65999 27.44668 21.980070 23.39864 21.78321
#> Merc 280         18.72157 18.82589 18.99172 17.249912 21.24348 18.30049
#> Honda Civic      29.66803 28.55722 31.92853 28.131520 27.31862 27.60294
#> Toyota Corolla   28.63992 27.65163 31.55401 30.084933 25.56220 26.69050
#> Camaro Z28       14.61451 14.34784 13.39836  8.986365 14.97222 14.66826
#> Pontiac Firebird 17.25657 14.82313 16.19174 12.794893 16.11565 16.05771
#> Maserati Bora    14.18150 14.71184 17.21216  8.396929 11.88185 14.61647
#>                     boot7    boot8    boot9   boot10   boot11   boot12   boot13
#> Mazda RX4        21.42965 18.82831 23.22819 20.14767 15.38615 21.52801 20.42563
#> Datsun 710       31.17257 25.62445 25.92730 24.50229 26.38631 26.26061 29.43843
#> Duster 360       13.31440 15.80368 15.89598 17.17782 15.19136 12.60124 10.22709
#> Merc 240D        23.64343 21.01089 23.67867 21.78253 24.44045 21.91810 26.32699
#> Merc 280         18.29764 17.98024 19.86190 17.58422 18.97526 17.93253 17.81826
#> Honda Civic      35.80988 25.13158 29.70004 26.70624 29.92394 30.08307 32.93965
#> Toyota Corolla   33.05723 27.56742 28.51666 26.01848 26.83720 27.57016 30.29603
#> Camaro Z28       17.50096 15.51962 14.91479 16.33376 16.78562 14.44168  9.86533
#> Pontiac Firebird 17.97528 15.62626 16.96378 16.31812 16.54070 16.04707 15.59535
#> Maserati Bora    20.20025 17.60688 13.24071 17.30561 19.69561 21.45137 17.86446
#>                    boot14   boot15   boot16   boot17   boot18   boot19   boot20
#> Mazda RX4        20.96366 23.00697 19.63938 20.72184 22.53705 21.92048 21.68905
#> Datsun 710       29.04419 26.01095 21.00265 25.77933 25.60174 25.00468 22.52004
#> Duster 360       12.14012 14.26750 16.01328 14.81375 15.41257 14.91257 15.08037
#> Merc 240D        22.16729 23.64211 19.49836 23.43581 22.98283 22.26822 21.15043
#> Merc 280         18.50003 19.07564 18.52614 19.48435 18.95140 18.24974 20.15770
#> Honda Civic      31.84528 27.46691 22.16310 27.58397 28.90394 27.96428 24.74005
#> Toyota Corolla   30.00382 27.00526 21.95821 26.47687 27.86820 27.03570 23.91624
#> Camaro Z28       13.48879 14.26750 15.80876 14.58134 14.54991 14.16212 14.90464
#> Pontiac Firebird 16.06674 16.75329 15.26705 15.22047 16.91750 15.85567 15.11960
#> Maserati Bora    24.75042 14.54118 16.73050 17.24715 14.33267 13.79177 14.54958

## Alternate Matrix Interface

# Train model
set.seed(123)
bolasso_mat <- bolasso(
  x = model.matrix(mpg ~ . - 1, mtcars_train),
  y = mtcars_train[, 1],
  data = mtcars_train,
  n.boot = 20,
  nfolds = 5
)

# Bagged ensemble prediction on test data
predict(bolasso_mat,
        new.data = model.matrix(mpg ~ . - 1, mtcars_test),
        select = "lambda.min")
#>                     boot1    boot2    boot3     boot4    boot5    boot6
#> Mazda RX4        23.53203 21.31155 20.38801 22.290720 22.27351 21.94408
#> Datsun 710       26.39838 25.65987 30.73719 27.331865 23.36205 24.69222
#> Duster 360       15.27134 15.34670 13.64426  8.423965 14.22472 15.46800
#> Merc 240D        23.13465 22.65999 27.44668 21.980070 23.39864 21.78321
#> Merc 280         18.72157 18.82589 18.99172 17.249912 21.24348 18.30049
#> Honda Civic      29.66803 28.55722 31.92853 28.131520 27.31862 27.60294
#> Toyota Corolla   28.63992 27.65163 31.55401 30.084933 25.56220 26.69050
#> Camaro Z28       14.61451 14.34784 13.39836  8.986365 14.97222 14.66826
#> Pontiac Firebird 17.25657 14.82313 16.19174 12.794893 16.11565 16.05771
#> Maserati Bora    14.18150 14.71184 17.21216  8.396929 11.88185 14.61647
#>                     boot7    boot8    boot9   boot10   boot11   boot12   boot13
#> Mazda RX4        21.42965 18.82831 23.22819 20.14767 15.38615 21.52801 20.42563
#> Datsun 710       31.17257 25.62445 25.92730 24.50229 26.38631 26.26061 29.43843
#> Duster 360       13.31440 15.80368 15.89598 17.17782 15.19136 12.60124 10.22709
#> Merc 240D        23.64343 21.01089 23.67867 21.78253 24.44045 21.91810 26.32699
#> Merc 280         18.29764 17.98024 19.86190 17.58422 18.97526 17.93253 17.81826
#> Honda Civic      35.80988 25.13158 29.70004 26.70624 29.92394 30.08307 32.93965
#> Toyota Corolla   33.05723 27.56742 28.51666 26.01848 26.83720 27.57016 30.29603
#> Camaro Z28       17.50096 15.51962 14.91479 16.33376 16.78562 14.44168  9.86533
#> Pontiac Firebird 17.97528 15.62626 16.96378 16.31812 16.54070 16.04707 15.59535
#> Maserati Bora    20.20025 17.60688 13.24071 17.30561 19.69561 21.45137 17.86446
#>                    boot14   boot15   boot16   boot17   boot18   boot19   boot20
#> Mazda RX4        20.96366 23.00697 19.63938 20.72184 22.53705 21.92048 21.68905
#> Datsun 710       29.04419 26.01095 21.00265 25.77933 25.60174 25.00468 22.52004
#> Duster 360       12.14012 14.26750 16.01328 14.81375 15.41257 14.91257 15.08037
#> Merc 240D        22.16729 23.64211 19.49836 23.43581 22.98283 22.26822 21.15043
#> Merc 280         18.50003 19.07564 18.52614 19.48435 18.95140 18.24974 20.15770
#> Honda Civic      31.84528 27.46691 22.16310 27.58397 28.90394 27.96428 24.74005
#> Toyota Corolla   30.00382 27.00526 21.95821 26.47687 27.86820 27.03570 23.91624
#> Camaro Z28       13.48879 14.26750 15.80876 14.58134 14.54991 14.16212 14.90464
#> Pontiac Firebird 16.06674 16.75329 15.26705 15.22047 16.91750 15.85567 15.11960
#> Maserati Bora    24.75042 14.54118 16.73050 17.24715 14.33267 13.79177 14.54958

# Extract the indices of the bootstrap replicates
bootstrap_samples(bolasso_mat)
#> $boot1
#>  [1]  3  3  4  5  5  7  8  9  9 10 10 11 14 14 15 18 19 19 19 20 22 22
#> 
#> $boot2
#>  [1]  2  5  6  7  7  7  8  9  9 10 10 11 12 12 13 13 14 15 17 21 21 21
#> 
#> $boot3
#>  [1]  1  2  4  6  6  7  8  9 11 15 15 16 16 17 17 18 18 20 21 22 22 22
#> 
#> $boot4
#>  [1]  3  3  3  5  5  7  8  8 12 13 14 14 14 15 16 19 19 20 21 22 22 22
#> 
#> $boot5
#>  [1]  2  3  7  8 10 10 10 11 12 12 14 14 14 15 15 17 17 18 19 20 22 22
#> 
#> $boot6
#>  [1]  2  4  5  6  6  7  7  7  9 10 11 12 13 14 14 16 16 19 19 20 21 22
#> 
#> $boot7
#>  [1]  1  3  4  4  7  8  8  8 10 11 11 12 15 16 16 17 20 20 20 20 22 22
#> 
#> $boot8
#>  [1]  1  2  4  6  8  8  8  8 10 11 12 13 13 13 14 14 14 16 18 21 21 21
#> 
#> $boot9
#>  [1]  1  5  7  7  7  9  9  9 10 10 11 11 11 13 14 14 19 20 20 21 22 22
#> 
#> $boot10
#>  [1]  1  1  1  2  3  4  5  6  6  7  9 10 10 14 17 17 17 18 20 20 21 21
#> 
#> $boot11
#>  [1]  1  5  6  7  7  8  8  9 10 13 13 16 17 17 18 18 20 21 21 21 21 21
#> 
#> $boot12
#>  [1]  2  2  2  3  3  3  3  4  5  6  6 10 10 10 12 13 13 16 16 17 17 18
#> 
#> $boot13
#>  [1]  1  2  2  2  3  4  5  7  7  9  9  9 11 11 12 12 13 15 16 19 21 22
#> 
#> $boot14
#>  [1]  3  3  6  6  8  8  9 10 10 12 13 15 16 17 17 18 18 19 20 20 20 22
#> 
#> $boot15
#>  [1]  1  2  4  5  7  8 10 10 10 11 12 13 13 16 17 17 18 18 19 19 19 22
#> 
#> $boot16
#>  [1]  1  1  2  3  4  6 11 12 12 12 14 15 15 16 16 16 17 18 19 19 21 21
#> 
#> $boot17
#>  [1]  5  6  6  7  8  9  9 10 10 10 11 11 12 12 13 17 18 18 18 19 20 21
#> 
#> $boot18
#>  [1]  3  4  4  4  4  6  6  7  8  9 10 10 14 15 16 16 17 19 20 20 21 22
#> 
#> $boot19
#>  [1]  2  5  5  6  7  8  9 10 11 11 14 15 15 15 16 16 16 19 19 20 20 22
#> 
#> $boot20
#>  [1]  2  3  5  6  6  6  7  8  8  8 10 11 14 14 15 15 17 17 18 19 20 21
#> 
```
