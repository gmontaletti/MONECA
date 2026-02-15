# Convert a square matrix to long format

Takes a square core matrix (no margins) and returns a data frame with
Origin, Destination, and Freq columns suitable for log-linear modelling.

## Usage

``` r
.matrix_to_long(mx)
```

## Arguments

- mx:

  A square numeric matrix with row and column names.

## Value

A data frame with columns Origin (factor), Destination (factor), and
Freq (numeric).
