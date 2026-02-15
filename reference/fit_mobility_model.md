# Fit Log-Linear Models to Mobility Tables

Fits standard log-linear models to mobility tables for statistical
testing of mobility structure. Supports independence,
quasi-independence, quasi-symmetry, uniform association, and MONECA
topology models.

## Usage

``` r
fit_mobility_model(
  mx,
  type = c("independence", "quasi_independence", "quasi_symmetry", "uniform_association",
    "moneca_topology"),
  level = NULL,
  scores = c("integer", "midrank"),
  small.cell.reduction = 0
)
```

## Arguments

- mx:

  A mobility matrix with margins (totals row/column), OR a moneca
  object. If a moneca object, the matrix is extracted from
  `mat.list[[1]]`.

- type:

  Character string specifying the model type. One of:

  `"independence"`

  :   No origin-destination association. Baseline with 2(n-1)
      parameters.

  `"quasi_independence"`

  :   Accounts for diagonal immobility cells. Adds n diagonal
      parameters. Standard mobility table baseline.

  `"quasi_symmetry"`

  :   Tests whether mobility O-\>D equals D-\>O. Adds n diagonal and
      n\*(n-1)/2 symmetric pair parameters.

  `"uniform_association"`

  :   Single-parameter association model using row/column scores.

  `"moneca_topology"`

  :   Tests whether MONECA segments explain mobility structure beyond
      diagonal effects. Requires a moneca object or explicit segment
      membership.

- level:

  Integer specifying the hierarchical level for MONECA topology model.
  Required when `type = "moneca_topology"` and `mx` is a moneca object.
  Default is NULL.

- scores:

  Character string specifying score type for uniform association model:
  `"integer"` (default) or `"midrank"`.

- small.cell.reduction:

  Numeric. Cells with counts below this value are set to zero before
  fitting. Default is 0 (no reduction).

## Value

An object of class `"moneca_loglinear"` containing:

- model:

  The `glm` fit object

- type:

  Model type string

- deviance:

  Residual deviance (G-squared)

- df:

  Residual degrees of freedom

- aic:

  Akaike Information Criterion

- bic:

  Bayesian Information Criterion

- p_value:

  P-value from chi-squared goodness-of-fit test

- dissimilarity_index:

  Proportion of cases misclassified

- coefficients:

  Named coefficient vector

- fitted:

  Fitted values reshaped as n x n matrix

- residuals:

  Pearson residuals reshaped as n x n matrix

- design_info:

  List describing the design matrix construction

- original_matrix:

  The input mobility matrix (with margins)

- segment_level:

  Level used (moneca_topology only, NULL otherwise)

## Details

All models are fit as Poisson log-linear models using
[`glm()`](https://rdrr.io/r/stats/glm.html). The deviance statistic
(G-squared) tests the goodness of fit relative to the saturated model. A
non-significant p-value indicates the model adequately describes the
observed mobility patterns.

The BIC is computed using the number of cells (n-squared) as the sample
size, which is the standard R approach for GLMs. For contingency table
comparisons based on total count N, users should refer to the deviance
and degrees of freedom directly.

The dissimilarity index indicates the proportion of individuals that
would need to be reclassified for the model to fit exactly. Values below
0.05 indicate good fit; below 0.03 indicates excellent fit.

## See also

[`compare_mobility_models`](https://gmontaletti.github.io/MONECA/reference/compare_mobility_models.md)
for model comparisons,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main clustering algorithm

## Examples

``` r
# Generate test data and run MONECA
mx <- generate_mobility_data(n_classes = 5, seed = 42)
seg <- moneca_fast(mx, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%

# Fit different models
m_ind <- fit_mobility_model(mx, type = "independence")
m_qi <- fit_mobility_model(mx, type = "quasi_independence")

# Fit MONECA topology model
m_top <- fit_mobility_model(seg, type = "moneca_topology", level = 2)

# Compare models
compare_mobility_models(m_ind, m_qi, m_top)
#> Log-Linear Model Comparison
#> ===========================
#> 
#>               model deviance df      aic      bic   p_value dissimilarity
#>  moneca_topology_L2  364.959  9  571.921  591.423  4.05e-73         0.046
#>  quasi_independence 1244.431 11 1447.393 1464.457 4.29e-260         0.077
#>        independence 9126.035 16 9318.997 9329.967  0.00e+00         0.392
#> 
#> Likelihood-Ratio Tests:
#>             model_1            model_2 delta_deviance delta_df   p_value
#>        independence quasi_independence       7881.604        5  0.00e+00
#>  quasi_independence moneca_topology_L2        879.472        2 1.06e-191
```
