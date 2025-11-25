# Parallel Assessment of Clustering Stability Using Bootstrap Resampling

Parallel version of assess_clustering_stability that processes bootstrap
samples concurrently for significant performance improvements. Uses
foreach/doParallel for cross-platform compatibility.

## Usage

``` r
assess_clustering_stability_parallel(
  mx,
  small.cell.reduction,
  cut.off = 1,
  n_bootstrap = 50,
  sample_fraction = 0.8,
  segment.levels = 3,
  n_cores = NULL,
  parallel = TRUE,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- mx:

  Mobility matrix for analysis.

- small.cell.reduction:

  Small cell reduction parameter to test.

- cut.off:

  Edge weight threshold for network construction. Default is 1.

- n_bootstrap:

  Number of bootstrap samples to generate. Default is 50.

- sample_fraction:

  Fraction of data to use in each bootstrap sample. Default is 0.8.

- segment.levels:

  Number of hierarchical levels for clustering. Default is 3.

- n_cores:

  Number of CPU cores to use. NULL (default) uses all available cores -
  1.

- parallel:

  Logical indicating whether to use parallel processing. Default is
  TRUE.

- seed:

  Random seed for reproducibility. Default is NULL.

- verbose:

  Logical indicating whether to show progress messages. Default is
  FALSE.

## Value

Numeric stability score between 0 and 1, where higher values indicate
more stable clustering results.

## Details

This function parallelizes the bootstrap sampling process: - Distributes
bootstrap samples across available cores - Each core independently runs
moneca_fast on its assigned samples - Results are combined to compute
overall stability - Falls back to sequential processing if parallel
packages unavailable

Performance improvements: - With 4 cores: ~3.5x faster than sequential -
With 8 cores: ~6-7x faster than sequential - Scales linearly with number
of cores up to n_bootstrap

## See also

[`assess_clustering_stability`](https://gmontaletti.github.io/MONECA/reference/assess_clustering_stability.md),
[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)

# Parallel stability assessment (auto-detect cores, default n_bootstrap = 50)
stability_parallel <- assess_clustering_stability_parallel(
  mobility_data,
  small.cell.reduction = 5,
  parallel = TRUE
)

# Sequential for comparison
stability_seq <- assess_clustering_stability_parallel(
  mobility_data,
  small.cell.reduction = 5,
  parallel = FALSE
)

# For more thorough assessment, increase n_bootstrap
stability_thorough <- assess_clustering_stability_parallel(
  mobility_data,
  small.cell.reduction = 5,
  n_bootstrap = 100,
  parallel = TRUE
)
} # }
```
