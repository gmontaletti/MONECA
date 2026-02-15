# Build design for uniform association model

Adds a single numeric column `score_interaction` = row_score \*
col_score. Scores are assigned based on the `scores` parameter.

## Usage

``` r
.build_design_uniform_association(df, categories, scores = "integer")
```

## Arguments

- df:

  Long-format data frame from `.matrix_to_long`.

- categories:

  Character vector of category names.

- scores:

  Character string: `"integer"` for 1, 2, ..., n or `"midrank"` for
  midrank values based on marginal distribution.

## Value

Data frame with added score_interaction column.
