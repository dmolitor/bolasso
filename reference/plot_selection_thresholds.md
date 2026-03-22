# Plot each covariate's smallest variable selection threshold

Plot the results of the [selection_thresholds](selection_thresholds.md)
function.

## Usage

``` r
plot_selection_thresholds(
  object = NULL,
  data = NULL,
  is_multinomial = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class [bolasso](bolasso.md) or `bolasso_fast`. This
  argument is optional if you directly pass in the data via the `data`
  argument. E.g. `data = selection_thresholds(object)`.

- data:

  A dataframe containing the selection thresholds. E.g. obtained via
  `selection_thresholds(object)`. This argument is optional if you
  directly pass a `bolasso` or `bolasso_fast` object via the `object`
  argument.

- is_multinomial:

  A boolean indicating if the data provided is from a multinomial
  regression model. Defaults to FALSE. This only needs to be provided if
  `data` is passed in directly. Otherwise this will be inferred from the
  model object.

- ...:

  Additional arguments to pass directly to
  [selection_thresholds](selection_thresholds.md).

## Value

A `ggplot` object

## See also

[`selection_thresholds()`](selection_thresholds.md)
