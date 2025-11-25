# Generate Data-Driven Candidate Values

Generates candidate values for small.cell.reduction parameter using
data-driven approaches including percentile analysis, matrix sparsity
assessment, and network density optimization.

## Usage

``` r
generate_candidate_values(
  mx,
  method = "hybrid",
  max_candidates = 20,
  include_zero = TRUE
)
```

## Arguments

- mx:

  Mobility matrix for analysis.

- method:

  Character string specifying generation strategy:

  - "percentile": Percentile-based thresholds from data distribution

  - "sparsity": Matrix sparsity-informed selection

  - "density": Network density optimization targets

  - "hybrid": Combination of multiple strategies (default)

- max_candidates:

  Maximum number of candidates to generate. Default is 20.

- include_zero:

  Logical indicating whether to include 0 as a candidate. Default is
  TRUE.

## Value

Numeric vector of candidate small.cell.reduction values, sorted in
ascending order.

## Details

**Percentile Method:** Generates thresholds based on the distribution of
non-zero values in the mobility matrix. Uses quantiles from 5

**Sparsity Method:** Analyzes matrix sparsity patterns and generates
candidates that preserve different levels of network connectivity.

**Density Method:** Targets specific network density levels (0.1, 0.2,
etc.) by estimating required thresholds.

**Hybrid Method:** Combines all approaches and selects diverse,
non-redundant candidates covering the full parameter space.

## See also

[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate sample data
mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)

# Percentile-based candidates
candidates_pct <- generate_candidate_values(mobility_data, "percentile")

# Sparsity-informed candidates  
candidates_sparse <- generate_candidate_values(mobility_data, "sparsity")

# Hybrid approach (recommended)
candidates_hybrid <- generate_candidate_values(mobility_data, "hybrid")

print(candidates_hybrid)
} # }
```
