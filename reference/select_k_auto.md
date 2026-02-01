# Automatic Component Selection

Selects the number of components k based on variance explained and elbow
detection.

## Usage

``` r
select_k_auto(mx, variance_target, verbose = FALSE)
```

## Arguments

- mx:

  Normalized matrix

- variance_target:

  Target cumulative variance (0-1)

- verbose:

  Print progress messages

## Value

List with k and variance_explained
