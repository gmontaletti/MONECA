# Compare Log-Linear Models for Mobility Tables

Compares multiple log-linear model fits for a mobility table, producing
a summary table with fit statistics and likelihood-ratio tests between
nested models.

## Usage

``` r
compare_mobility_models(..., mx = NULL, level = NULL, include_topology = TRUE)
```

## Arguments

- ...:

  One or more `moneca_loglinear` objects to compare.

- mx:

  Optional. A mobility matrix with margins or a moneca object. If
  provided, fits all standard models automatically for comparison.

- level:

  Integer specifying the hierarchical level for the MONECA topology
  model. Used when `mx` is a moneca object.

- include_topology:

  Logical. Whether to include the MONECA topology model when fitting all
  models via `mx`. Default is TRUE. Set to FALSE if `mx` is a plain
  matrix without segment information.

## Value

An object of class `"moneca_model_comparison"` containing:

- table:

  Data frame with columns: model, deviance, df, aic, bic, p_value,
  dissimilarity

- lr_tests:

  Data frame of pairwise likelihood-ratio tests between nested models,
  with columns: model_1, model_2, delta_deviance, delta_df, p_value

- models:

  Named list of the fitted model objects

## Details

When `mx` is provided, the function fits independence,
quasi-independence, quasi-symmetry, and uniform association models
automatically. If `mx` is a moneca object and `include_topology = TRUE`,
a MONECA topology model is also fit.

Likelihood-ratio tests are computed for known nesting relationships:

- Independence is nested in quasi-independence

- Independence is nested in uniform association

- Quasi-independence is nested in quasi-symmetry

- Quasi-independence is nested in MONECA topology

Models passed via `...` are combined with any automatically fitted
models. Duplicate model types are resolved by keeping the explicitly
passed version.

## See also

[`fit_mobility_model`](https://gmontaletti.github.io/MONECA/reference/fit_mobility_model.md)
for fitting individual models

## Examples

``` r
mx <- generate_mobility_data(n_classes = 5, seed = 42)
seg <- moneca_fast(mx, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%

# Compare all standard models
comp <- compare_mobility_models(mx = seg, level = 2)
print(comp)
#> Log-Linear Model Comparison
#> ===========================
#> 
#>                model deviance df      aic      bic   p_value dissimilarity
#>       quasi_symmetry   12.256  6  225.218  248.377  5.65e-02         0.006
#>      moneca_topology  364.959  9  571.921  591.423  4.05e-73         0.046
#>   quasi_independence 1244.431 11 1447.393 1464.457 4.29e-260         0.077
#>  uniform_association 1432.361 15 1627.323 1639.511 1.80e-296         0.133
#>         independence 9126.035 16 9318.997 9329.967  0.00e+00         0.392
#> 
#> Likelihood-Ratio Tests:
#>             model_1             model_2 delta_deviance delta_df   p_value
#>        independence  quasi_independence       7881.604        5  0.00e+00
#>        independence uniform_association       7693.675        1  0.00e+00
#>  quasi_independence      quasi_symmetry       1232.174        5 3.15e-264
#>  quasi_independence     moneca_topology        879.472        2 1.06e-191

# Compare specific models
m1 <- fit_mobility_model(mx, type = "independence")
m2 <- fit_mobility_model(mx, type = "quasi_independence")
compare_mobility_models(m1, m2)
#> Log-Linear Model Comparison
#> ===========================
#> 
#>               model deviance df      aic      bic   p_value dissimilarity
#>  quasi_independence 1244.431 11 1447.393 1464.457 4.29e-260         0.077
#>        independence 9126.035 16 9318.997 9329.967  0.00e+00         0.392
#> 
#> Likelihood-Ratio Tests:
#>       model_1            model_2 delta_deviance delta_df p_value
#>  independence quasi_independence       7881.604        5       0
```
