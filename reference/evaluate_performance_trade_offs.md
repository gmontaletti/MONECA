# Evaluate Performance Trade-offs

Balances clustering quality against computational performance to
identify optimal parameters for different use cases and constraints.

## Usage

``` r
evaluate_performance_trade_offs(
  quality_metrics,
  performance_metrics,
  performance_weight = 0.3
)
```

## Arguments

- quality_metrics:

  List of quality metric results for each candidate.

- performance_metrics:

  List of performance metric results for each candidate.

- performance_weight:

  Numeric weight (0-1) for performance vs quality trade-off. Higher
  values prioritize speed over quality.

## Value

Numeric vector of trade-off scores for each candidate.

## Details

The function combines normalized quality and performance scores using
the specified weight. Quality scores include clustering metrics like
modularity and silhouette coefficients. Performance scores consider
computational time and memory efficiency.

## See also

[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# This function is typically called internally by auto_tune_small_cell_reduction
# Example usage in isolation:

quality_list <- list(
  list(overall = 0.8),
  list(overall = 0.6),
  list(overall = 0.9)
)

performance_list <- list(
  list(time = 1.2, valid = TRUE),
  list(time = 0.8, valid = TRUE), 
  list(time = 2.1, valid = TRUE)
)

trade_offs <- evaluate_performance_trade_offs(
  quality_metrics = quality_list,
  performance_metrics = performance_list,
  performance_weight = 0.3
)
} # }
```
