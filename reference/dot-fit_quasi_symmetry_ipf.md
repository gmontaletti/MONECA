# Fit quasi-symmetry model via Iterative Proportional Fitting

Computes the MLE for the quasi-symmetry log-linear model using IPF
instead of GLM. All operations are vectorized (no R-level loops over
cells). Convergence is guaranteed and typically reached in 20-50
iterations.

## Usage

``` r
.fit_quasi_symmetry_ipf(core, max_iter = 500L, tol = 1e-08)
```

## Arguments

- core:

  An N x N numeric matrix (no margins) of observed counts.

- max_iter:

  Maximum number of IPF iterations. Default 500.

- tol:

  Convergence tolerance on the maximum absolute deviation between fitted
  and observed marginal totals. Default 1e-8.

## Value

A list with components: fitted, deviance, df, p_value, aic, bic,
dissimilarity_index, residuals, n_iter, converged.
