# Extract indices used for each bootstrap replicate

This function extracts the indices used to bootstrap the data for each
replicate. This is helpful when doing some additional analysis that
requires knowledge of the exact bootstrap samples used.

## Usage

``` r
bootstrap_samples(model)
```

## Arguments

- model:

  A model of class [bolasso](bolasso.md).

## Value

A named list of length equal to the number of bootstrap replicates that
contains the corresponding indices used to subsample the data.
