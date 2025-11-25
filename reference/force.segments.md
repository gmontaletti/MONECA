# Force Segments

Creates a two-level segment object with a forced solution based on an
external variable. This is an internal function for creating custom
segmentation structures.

## Usage

``` r
force.segments(segments, variable)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- variable:

  A vector specifying forced segment assignments.

## Value

A moneca object with forced segmentation at level 2.
