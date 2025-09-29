# bolasso 0.4.0

- Allows the user to extract the bootstrap indices with `bootstrap_samples()`.

# bolasso 0.3.0

## New Features

- **Fast Estimation Mode**: 
  - `bolasso()` gains a `fast` argument which optimizes computation by using a single cross-validated regression on the entire dataset to determine the optimal regularization parameter (`lambda`). This approach bypasses the need for cross-validation within each bootstrap replicate, drastically reducing computation time, especially beneficial for large datasets or when using a high number of bootstrap replicates.
    ```r
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
  - `selected_vars()` is now a shorthand for `selected_variables()`.
  - `selected_variables()`/`selected_vars()` supports two variable selection algorithms via the `method` argument: the Variable Inclusion Probability (VIP) method and the Quantile (QNT) method. The VIP method selects variables that appear in a high percentage of bootstrap models, while the QNT method selects variables based on bootstrap confidence intervals. Set `method = "vip"` or `method = "qnt"`, respectively.
    ```r
    # Select variables using the VIP method with a 95% threshold
    selected_vars_vip <- selected_variables(model, threshold = 0.95, method = "vip")

    # Select variables using the QNT method
    selected_vars_qnt <- selected_variables(model, threshold = 0.95, method = "qnt")
    ```

- **Tidy Method for Bolasso Objects**:
  - `tidy()` extracts a tidy tibble summarizing bootstrap-level coefficients for each covariate. This method provides a clean and organized way to inspect model coefficients.
    ```r
    # Extract a tidy tibble of coefficients
    tidy_coefs <- tidy(model, select = "lambda.min")
    ```

- **Variable Selection Visualization**: 
  - `plot_selection_thresholds()` provides a visual representation of the selection thresholds for each variable. This visualization helps users understand the stability and robustness of variable selection across different thresholds and methods.
    ```r
    # Visualize selection thresholds for variables
    plot_selection_thresholds(model, select = "lambda.min")
    ```

## Improvements

- **Plotting Coefficient Distributions**:
  - `plot_selected_variables()` visualizes the coefficient distributions for only the selected variables. This function provides a focused view of the most relevant variables in the model.
    ```r
    # Plot coefficient distributions for selected variables
    plot_selected_variables(
    model,
    threshold = 0.95,
    method = "vip",
    select = "lambda.min"
    )
    ```

  - `plot` visualizes the coefficient distributions for all model covariates.
    ```r
    # Plot coefficient distributions for selected variables
    plot(model, select = "lambda.min")
    ```

# bolasso 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* `bolasso()` argument `form` has been renamed to `formula` to reflect common naming conventions in R statistical modeling packages.
* `predict()` and `coef()` methods are now implemented using `future.apply::future_lapply` allowing for computing predictions and extracting coefficients in parallel. This may result in slightly worse performance (due to memory overhead) when the model/prediction data is small but will be significantly faster when e.g. generating predictions on a very large data-set.
* Solved an issue with `bolasso()` argument `formula`. The user-supplied value of `formula` is handled via `deparse()` which has a default `width.cutoff` value of 60. This was causing issues with formulas by splitting them into multi-element character vectors. It has now been set to the maximum value of `500L` which will correctly parse all lengths of formulas.
* `predict()` now forces evaluation of the `formula` argument in the `bolasso()` call. This resolves an issue where, if a user passes a formula via a variable, `predict()` would pass the variable name to the underlying prediction function as opposed to the actual formula.
