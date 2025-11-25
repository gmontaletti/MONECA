# Enhanced Automatic Small Cell Reduction Parameter Tuning with Multi-Objective Optimization

Automatically selects optimal small.cell.reduction parameter for MONECA
clustering using advanced optimization methods including Pareto frontier
analysis, cross-validation, Bayesian optimization, and performance-aware
tuning. This enhanced version provides significant speed improvements
while maintaining or improving tuning quality.

## Usage

``` r
auto_tune_small_cell_reduction(
  mx,
  cut.off = 1,
  method = "quality",
  n_trials = 5,
  candidate_values = NULL,
  performance_weight = 0.3,
  min_density = 0.01,
  max_candidates = 20,
  seed = NULL,
  verbose = FALSE,
  use_cache = TRUE,
  parallel = "auto",
  early_stopping = TRUE,
  pareto_objectives = c("quality", "performance"),
  cv_folds = 5,
  bayesian_iterations = 20
)
```

## Arguments

- mx:

  Mobility matrix to analyze. Must be a square numeric matrix
  representing mobility flows between categories.

- cut.off:

  Numeric threshold for edge weights in network construction. Default is
  1.

- method:

  Character string specifying tuning strategy. Options are:

  - "quality" (default, RECOMMENDED): Fast clustering quality
    optimization (10x faster than stability)

  - "stability": Bootstrap-based stability assessment (more thorough but
    slower)

  - "performance": Balance between quality and computational efficiency

  - "pareto": Multi-objective optimization with Pareto frontier analysis

  - "cross_validation": Cross-validation based parameter selection

  - "bayesian": Bayesian optimization (requires GPfit package)

- n_trials:

  Integer number of trials for stability assessment or CV folds. Default
  is 5. Increase to 10-20 for more thorough assessment at cost of speed.

- candidate_values:

  Numeric vector of candidate values to test. If NULL, values are
  generated automatically using data-driven methods.

- performance_weight:

  Numeric weight (0-1) for performance vs quality trade-off. Higher
  values prioritize speed over quality. Default is 0.3.

- min_density:

  Minimum acceptable network density threshold. Default is 0.01.

- max_candidates:

  Maximum number of candidate values to evaluate for computational
  efficiency. Default is 20.

- seed:

  Integer seed for reproducible results in bootstrap sampling. Default
  is NULL.

- verbose:

  Logical indicating whether to show progress messages. Default is
  FALSE.

- use_cache:

  Logical indicating whether to use caching for repeated computations.
  Default is TRUE.

- parallel:

  Character or logical indicating parallel processing preference. Can be
  "auto" (default, intelligent switching), TRUE/FALSE (force
  parallel/sequential), or "parallel"/"sequential" for explicit control.

- early_stopping:

  Logical indicating whether to use early stopping for candidate
  screening. Default is TRUE.

- pareto_objectives:

  Character vector of objectives for Pareto optimization. Default is
  c("quality", "performance").

- cv_folds:

  Integer number of cross-validation folds. Default is 5.

- bayesian_iterations:

  Integer number of Bayesian optimization iterations. Default is 20.

## Value

A list containing:

- optimal_value:

  Recommended small.cell.reduction parameter value

- stability_scores:

  Stability assessment results for each candidate

- quality_metrics:

  Clustering quality metrics for each candidate

- performance_metrics:

  Computational performance metrics

- candidates_tested:

  Vector of candidate values evaluated

- tuning_method:

  Method used for parameter selection

- selection_rationale:

  Text explanation of parameter selection

- network_properties:

  Network density and connectivity metrics

- computation_time:

  Total time spent on parameter tuning

## Details

The function implements three tuning strategies:

**Stability Method:** Uses bootstrap resampling to assess clustering
consistency across data subsamples. Parameters that produce more stable
clustering structures are preferred.

**Quality Method:** Optimizes clustering quality using multiple metrics
including silhouette coefficients, network modularity, and
information-theoretic measures.

**Performance Method:** Balances clustering quality with computational
efficiency, considering both analysis time and memory requirements.

Candidate values are automatically generated using percentile-based
thresholds, matrix sparsity analysis, and network density targets unless
explicitly provided.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
main clustering function,
[`weight.matrix`](https://gmontaletti.github.io/MONECA/reference/weight.matrix.md)
for weight matrix computation,
[`generate_candidate_values`](https://gmontaletti.github.io/MONECA/reference/generate_candidate_values.md)
for candidate generation,
[`assess_clustering_stability`](https://gmontaletti.github.io/MONECA/reference/assess_clustering_stability.md)
for stability assessment

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample mobility data
mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)

# Quality-based tuning (default, fast and recommended)
tuning_result <- auto_tune_small_cell_reduction(
  mx = mobility_data,
  verbose = TRUE
)

# Use optimal parameter in moneca analysis
segments <- moneca(mobility_data,
                  small.cell.reduction = tuning_result$optimal_value)

# Stability-based tuning (slower but more thorough)
stability_result <- auto_tune_small_cell_reduction(
  mx = mobility_data,
  method = "stability",
  n_trials = 10,
  verbose = TRUE
)

# Quality-based tuning with custom candidates
custom_result <- auto_tune_small_cell_reduction(
  mx = mobility_data,
  candidate_values = c(0, 1, 2, 5, 10, 20),
  verbose = TRUE
)

# Performance-aware tuning
performance_result <- auto_tune_small_cell_reduction(
  mx = mobility_data,
  method = "performance",
  performance_weight = 0.5,  # Equal weight to speed and quality
  verbose = TRUE
)
} # }
```
