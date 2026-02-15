# Build design for MONECA topology model

Adds diagonal indicators and a segment-pair factor encoding
within-segment and between-segment relationships based on MONECA segment
membership. Uses a single factor variable for numerical stability with
large matrices.

## Usage

``` r
.build_design_moneca_topology(df, categories, segment_membership)
```

## Arguments

- df:

  Long-format data frame from `.matrix_to_long`.

- categories:

  Character vector of category names.

- segment_membership:

  Named character vector mapping category name to segment label (e.g.,
  `"seg_1"`, `"seg_2"`).

## Value

Data frame with diagonal indicators and a seg_pair factor column.
