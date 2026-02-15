# Compute dissimilarity index between observed and fitted values

The dissimilarity index measures the proportion of cases that would need
to be reclassified for the model to fit exactly.

## Usage

``` r
.dissimilarity_index(observed, fitted)
```

## Arguments

- observed:

  Numeric vector of observed counts.

- fitted:

  Numeric vector of fitted (expected) counts.

## Value

A single numeric value between 0 and 1.
