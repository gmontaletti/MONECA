# Compute Multi-Objective Optimization Metrics

Computes comprehensive metrics for evaluating clustering quality,
including silhouette analysis, network modularity, and
information-theoretic measures.

## Usage

``` r
compute_tuning_metrics(
  segments = NULL,
  weight_matrix = NULL,
  include_silhouette = TRUE,
  include_modularity = TRUE
)
```

## Arguments

- segments:

  MONECA segmentation results (optional, for full analysis).

- weight_matrix:

  Weight matrix from mobility data (required).

- include_silhouette:

  Logical indicating whether to compute silhouette coefficients. Default
  is TRUE.

- include_modularity:

  Logical indicating whether to compute network modularity. Default is
  TRUE.

## Value

List containing:

- silhouette:

  Average silhouette coefficient

- modularity:

  Network modularity score

- network_density:

  Network density measure

- connectivity:

  Network connectivity metrics

- overall:

  Overall quality score combining all metrics

## Details

The function evaluates clustering quality using multiple complementary
metrics:

**Silhouette Analysis:** Measures how well-separated clusters are by
comparing within-cluster and between-cluster distances.

**Network Modularity:** Assesses community structure quality in the
mobility network.

**Connectivity Metrics:** Evaluates network structure properties
including density, component structure, and edge distribution.

## See also

[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data and create weight matrix
mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
weight_matrix <- weight.matrix(mobility_data, small.cell.reduction = 2)

# Compute quality metrics
quality <- compute_tuning_metrics(weight_matrix = weight_matrix)
print(quality)
} # }
```
