# Generate Colors for Segments

Creates a grayscale color scheme for MONECA segments based on internal
mobility rates. Darker colors indicate higher immobility (lower internal
mobility).

## Usage

``` r
segment.colors(segments)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

## Value

A list of color vectors, one for each hierarchical level.

## Details

This function calculates grayscale colors where the intensity reflects
the immobility rate within each segment. Segments with higher immobility
(more stable positions) receive darker colors.
