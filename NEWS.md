# bolasso 0.2.0

* Added a `NEWS.md` file to track changes to the package.
* `bolasso()` argument `form` has been renamed to `formula` to reflect common naming conventions in R statistical modeling packages.
* `predict()` and `coef()` methods are now implemented using `future.apply::future_lapply` allowing for computing predictions and extracting coefficients in parallel. This may result in slightly worse performance (due to memory overhead) when the model/prediction data is small but will be significantly faster when e.g. generating predictions on a very large data-set.
* Solved an issue with `bolasso()` argument `formula`. The user-supplied value of `formula` is handled via `deparse()` which has a default `width.cutoff` value of 60. This was causing issues with formulas by splitting them into multi-element character vectors. It has now been set to the maximum value of `500L` which will correctly parse all lengths of formulas.
* `predict()` now forces evaluation of the `formula` argument in the `bolasso()` call. This resolves an issue where, if a user passes a formula via a variable, `predict()` would pass the variable name to the underlying prediction function as opposed to the actual formula.
