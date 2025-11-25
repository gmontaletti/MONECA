# Assess Clustering Stability Using Bootstrap Resampling

Evaluates the stability of MONECA clustering results for a given
small.cell.reduction parameter using bootstrap resampling. More stable
parameters produce consistent clustering across data subsamples.

## Usage

``` r
assess_clustering_stability(
  mx,
  small.cell.reduction,
  cut.off = 1,
  n_bootstrap = 50,
  sample_fraction = 0.8,
  segment.levels = 3
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

## Value

Numeric stability score between 0 and 1, where higher values indicate
more stable clustering results.

## Details

The function performs bootstrap resampling by: 1. Creating multiple
subsamples of the mobility matrix 2. Running MONECA clustering on each
subsample 3. Extracting clustering signatures (segment memberships) 4.
Computing stability metrics based on clustering consistency

The stability score combines multiple measures including: - Consistency
of segment assignments across bootstrap samples - Stability of
hierarchical structure - Robustness of clique detection

## See also

[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md),
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)

# Assess stability for different parameters (uses default n_bootstrap = 50)
stability_0 <- assess_clustering_stability(mobility_data,
                                          small.cell.reduction = 0)
stability_5 <- assess_clustering_stability(mobility_data,
                                          small.cell.reduction = 5)

cat("Stability with parameter 0:", stability_0, "\n")
cat("Stability with parameter 5:", stability_5, "\n")

# For more thorough assessment, increase n_bootstrap
stability_thorough <- assess_clustering_stability(mobility_data,
                                                  small.cell.reduction = 2,
                                                  n_bootstrap = 100)
} # }
```
