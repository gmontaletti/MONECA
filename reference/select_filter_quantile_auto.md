# Automatic Filter Quantile Selection for NMF

Selects the filter quantile for NMF based on elbow detection on sorted
reconstruction values. The elbow point marks where "signal" transitions
to "noise" - cells beyond this point contribute little to the overall
structure captured by NMF.

## Usage

``` r
select_filter_quantile_auto(reconstruction, verbose = FALSE)
```

## Arguments

- reconstruction:

  NMF reconstruction matrix (W \* H)

- verbose:

  Print progress messages

## Value

Numeric quantile value (proportion to keep, e.g., 0.75 = keep top 75
