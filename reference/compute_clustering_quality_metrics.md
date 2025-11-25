# Compute Clustering Quality Metrics

Convenience wrapper for compute_tuning_metrics focused on clustering
quality.

## Usage

``` r
compute_clustering_quality_metrics(weight_matrix, segments = NULL)
```

## Arguments

- weight_matrix:

  Weight matrix from mobility data.

- segments:

  Optional MONECA segmentation results.

## Value

List of clustering quality metrics.

## See also

[`compute_tuning_metrics`](https://gmontaletti.github.io/MONECA/reference/compute_tuning_metrics.md)
