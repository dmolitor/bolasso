# Changelog

## bolasso 0.5.0

CRAN release: 2026-03-19

- Addresses \[[\#19](https://github.com/dmolitor/bolasso/issues/19)\].
  When `family = "multinomial"` is specified, previously bolasso did not
  correctly support variable selection, plotting, etc. This version
  addresses this. In general, when `family = "multinomial"`, all outputs
  become a list where each list element corresponds to a unique outcome
  in the multinomial response vector. However, the content of each list
  element will be the same as it originally was (e.g. a tibble of
  coefficients, or a vector of variable names, etc.). In the case of
  plotting, it will create a facetted plot with one facet per unique
  outcome. The only limitations are that `family = "mgaussian"` is not
  currently supported, and when predicting with
  `family = "multinomial"`, only `type = "class"` is currently
  supported. These may be added at a later date.

## bolasso 0.4.0

CRAN release: 2025-10-14

- Allows the user to extract the bootstrap indices with
  [`bootstrap_samples()`](../reference/bootstrap_samples.md).

      library(bolasso)

      model <- bolasso(mpg ~ hp + wt, data = mtcars, n.boot = 10)
      bootstrap_samples(model)

- Fixes [\#12](https://github.com/dmolitor/bolasso/issues/12)

- Fixes [\#13](https://github.com/dmolitor/bolasso/issues/13)

## bolasso 0.3.0

CRAN release: 2024-12-08

### New Features

- **Fast Estimation Mode**:
  - [`bolasso()`](../reference/bolasso.md) gains a `fast` argument which
    optimizes computation by using a single cross-validated regression
    on the entire dataset to determine the optimal regularization
    parameter (`lambda`). This approach bypasses the need for
    cross-validation within each bootstrap replicate, drastically
    reducing computation time, especially beneficial for large datasets
    or when using a high number of bootstrap replicates.

    ``` r
    # Fast mode reduces computation time by using a single cross-validated lambda
    model_fast <- bolasso(
    diabetes ~ .,
    data = train,
    n.boot = 1000,
    progress = FALSE,
    family = "binomial",
    fast = TRUE
    )
    ```
- **Enhanced Variable Selection Methods**:
  - [`selected_vars()`](../reference/selected_variables.md) is now a
    shorthand for
    [`selected_variables()`](../reference/selected_variables.md).

  - [`selected_variables()`](../reference/selected_variables.md)/[`selected_vars()`](../reference/selected_variables.md)
    supports two variable selection algorithms via the `method`
    argument: the Variable Inclusion Probability (VIP) method and the
    Quantile (QNT) method. The VIP method selects variables that appear
    in a high percentage of bootstrap models, while the QNT method
    selects variables based on bootstrap confidence intervals. Set
    `method = "vip"` or `method = "qnt"`, respectively.

    ``` r
    # Select variables using the VIP method with a 95% threshold
    selected_vars_vip <- selected_variables(model, threshold = 0.95, method = "vip")

    # Select variables using the QNT method
    selected_vars_qnt <- selected_variables(model, threshold = 0.95, method = "qnt")
    ```
- **Tidy Method for Bolasso Objects**:
  - [`tidy()`](https://generics.r-lib.org/reference/tidy.html) extracts
    a tidy tibble summarizing bootstrap-level coefficients for each
    covariate. This method provides a clean and organized way to inspect
    model coefficients.

    ``` r
    # Extract a tidy tibble of coefficients
    tidy_coefs <- tidy(model, select = "lambda.min")
    ```
- **Variable Selection Visualization**:
  - [`plot_selection_thresholds()`](../reference/plot_selection_thresholds.md)
    provides a visual representation of the selection thresholds for
    each variable. This visualization helps users understand the
    stability and robustness of variable selection across different
    thresholds and methods.

    ``` r
    # Visualize selection thresholds for variables
    plot_selection_thresholds(model, select = "lambda.min")
    ```

### Improvements

- **Plotting Coefficient Distributions**:
  - [`plot_selected_variables()`](../reference/plot_selected_variables.md)
    visualizes the coefficient distributions for only the selected
    variables. This function provides a focused view of the most
    relevant variables in the model.

    ``` r
    # Plot coefficient distributions for selected variables
    plot_selected_variables(
    model,
    threshold = 0.95,
    method = "vip",
    select = "lambda.min"
    )
    ```

  - `plot` visualizes the coefficient distributions for all model
    covariates.

    ``` r
    # Plot coefficient distributions for selected variables
    plot(model, select = "lambda.min")
    ```

## bolasso 0.2.0

CRAN release: 2022-05-09

- Added a `NEWS.md` file to track changes to the package.
- [`bolasso()`](../reference/bolasso.md) argument `form` has been
  renamed to `formula` to reflect common naming conventions in R
  statistical modeling packages.
- [`predict()`](https://rdrr.io/r/stats/predict.html) and
  [`coef()`](https://rdrr.io/r/stats/coef.html) methods are now
  implemented using
  [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html)
  allowing for computing predictions and extracting coefficients in
  parallel. This may result in slightly worse performance (due to memory
  overhead) when the model/prediction data is small but will be
  significantly faster when e.g. generating predictions on a very large
  data-set.
- Solved an issue with [`bolasso()`](../reference/bolasso.md) argument
  `formula`. The user-supplied value of `formula` is handled via
  [`deparse()`](https://rdrr.io/r/base/deparse.html) which has a default
  `width.cutoff` value of 60. This was causing issues with formulas by
  splitting them into multi-element character vectors. It has now been
  set to the maximum value of `500L` which will correctly parse all
  lengths of formulas.
- [`predict()`](https://rdrr.io/r/stats/predict.html) now forces
  evaluation of the `formula` argument in the
  [`bolasso()`](../reference/bolasso.md) call. This resolves an issue
  where, if a user passes a formula via a variable,
  [`predict()`](https://rdrr.io/r/stats/predict.html) would pass the
  variable name to the underlying prediction function as opposed to the
  actual formula.
