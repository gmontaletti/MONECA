# Log-Linear Models for Mobility Tables

## Introduction

A mobility table records how individuals move between categories
(occupations, social classes, regions) over time. MONECA identifies
clusters of categories with high internal mobility using a
graph-theoretic algorithm. Log-linear models complement this by
providing a statistical framework to test whether those clusters — or
other structural features — capture genuine patterns in the data.

The moneca package provides two functions for this purpose:

- [`fit_mobility_model()`](https://gmontaletti.github.io/MONECA/reference/fit_mobility_model.md)
  fits a single log-linear model to a mobility table
- [`compare_mobility_models()`](https://gmontaletti.github.io/MONECA/reference/compare_mobility_models.md)
  compares multiple models side by side, with likelihood-ratio tests for
  nested pairs

This vignette walks through a complete analysis using synthetic data,
covering the five model types, how to read their output, how to compare
them, and how to inspect residuals for further insight.

## Setup: generate data and run MONECA

``` r
library(moneca)

# Generate an 8-class mobility table
mx <- generate_mobility_data(
  n_classes = 8,
  n_total = 10000,
  immobility_strength = 0.6,
  class_clustering = 0.3,
  seed = 42
)

# Run MONECA
seg <- moneca_fast(mx, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%
print(seg)
#> 
#> ================================================================================
#>                         moneca MOBILITY ANALYSIS RESULTS                        
#> ================================================================================
#> 
#> OVERALL MOBILITY PATTERNS
#> -------------------------------------------------------------------------------
#> Overall Population Mobility Rate:                    41.3%
#> Average Mobility Concentration (all levels):         88.8%
#> 
#> HIERARCHICAL SEGMENTATION ANALYSIS
#> -------------------------------------------------------------------------------
#> 
#> Internal Mobility Within Segments (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    58.7    82.9    90.2   100.0 
#> 
#> Mobility Concentration in Significant Pathways by Level (%):
#> Level 1 Level 2 Level 3 Level 4 
#>    82.0    82.9    90.2   100.0 
#> 
#> Network Structure by Level:
#>                                    Level 1      Level 2      Level 3      Level 4 
#> -------------------------------------------------------------------------------
#> Active Segments/Classes:                 8            3            2            1 
#> Significant Edges:                      13            0            0            0 
#> Network Density:                     0.232        0.000        0.000          NaN 
#> Isolated Segments:                       0            3            2            1 
#> 
#> DETAILED WEIGHTED DEGREE DISTRIBUTIONS (STRENGTH)
#> -------------------------------------------------------------------------------
#> 
#> Total Weighted Connections (Strength In + Out):
#>          Min   Q1 Median Mean   Q3  Max
#> Level 1 1.23 2.43   5.68  5.1 6.59 10.5
#> Level 2 0.00 0.00   0.00  0.0 0.00  0.0
#> Level 3 0.00 0.00   0.00  0.0 0.00  0.0
#> Level 4 0.00 0.00   0.00  0.0 0.00  0.0
#> 
#> Outward Mobility Strength (Weighted Out-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 2.39   2.67 2.55 3.16 4.02
#> Level 2   0 0.00   0.00 0.00 0.00 0.00
#> Level 3   0 0.00   0.00 0.00 0.00 0.00
#> Level 4   0 0.00   0.00 0.00 0.00 0.00
#> 
#> Inward Mobility Strength (Weighted In-Degree):
#>         Min   Q1 Median Mean   Q3  Max
#> Level 1   0 0.92    2.7 2.55 3.11 7.51
#> Level 2   0 0.00    0.0 0.00 0.00 0.00
#> Level 3   0 0.00    0.0 0.00 0.00 0.00
#> Level 4   0 0.00    0.0 0.00 0.00 0.00
#> 
#> Edge Weight Distribution (Relative Risk Values):
#>          Min  Q1 Median Mean   Q3  Max
#> Level 1 1.21 1.3    1.4 1.57 1.78 2.56
#> Level 2   NA  NA     NA  NaN   NA   NA
#> Level 3   NA  NA     NA  NaN   NA   NA
#> Level 4   NA  NA     NA  NaN   NA   NA
#> 
#> ================================================================================
```

The resulting object `seg` contains segment assignments at multiple
hierarchical levels. The log-linear models below test whether these
assignments explain statistically meaningful structure in the table.

## The five model types

Each model represents a different hypothesis about what drives mobility
patterns. They are ordered from simplest to most complex.

### Independence

The independence model assumes that origin and destination are
unrelated: knowing where someone starts tells you nothing about where
they end up. All cell frequencies are predicted from the row and column
marginal totals alone.

``` r
m_ind <- fit_mobility_model(mx, type = "independence")
print(m_ind)
#> Log-Linear Model for Mobility Table
#> ====================================
#> Type: independence
#> Categories: 8
#> 
#> Deviance (G2): 14676.049
#> Df: 49
#> P-value: 0
#> Dissimilarity index: 0.500
#> AIC: 15068.707
#> BIC: 15101.091
```

This model almost always fits poorly for mobility tables, because people
tend to stay in their current position or move to nearby ones. It serves
as a baseline against which other models are compared.

### Quasi-independence

The quasi-independence model adds a separate parameter for each diagonal
cell, accounting for the fact that people tend to stay put (immobility).
Off-diagonal flows are still assumed to follow the marginal
distributions.

``` r
m_qi <- fit_mobility_model(mx, type = "quasi_independence")
print(m_qi)
#> Log-Linear Model for Mobility Table
#> ====================================
#> Type: quasi_independence
#> Categories: 8
#> 
#> Deviance (G2): 3843.491
#> Df: 41
#> P-value: 0
#> Dissimilarity index: 0.176
#> AIC: 4252.149
#> BIC: 4301.803
```

The improvement over independence is typically large, because immobility
is the dominant feature of most mobility tables. However, a low p-value
or high dissimilarity index here means that the off-diagonal structure —
who moves where — is not random.

### Quasi-symmetry

The quasi-symmetry model tests whether the flow from category A to B
equals the flow from B to A, after accounting for differences in
marginal sizes. It adds a parameter for each unordered pair of
categories.

``` r
m_qs <- fit_mobility_model(mx, type = "quasi_symmetry")
print(m_qs)
#> Log-Linear Model for Mobility Table
#> ====================================
#> Type: quasi_symmetry
#> Categories: 8
#> 
#> Deviance (G2): 19.811
#> Df: 21
#> P-value: 0.533
#> Dissimilarity index: 0.008
#> AIC: 468.469
#> BIC: 561.301
```

If this model fits well, mobility is approximately symmetric:
transitions between any two categories are balanced (relative to their
sizes). If it fits poorly, some flows are directional — people move more
easily in one direction than the other.

The quasi-symmetry model is fitted using Iterative Proportional Fitting
(IPF) rather than GLM, which makes it fast and numerically stable even
for large matrices.

### Uniform association

The uniform association model adds a single parameter measuring the
overall strength of ordinal association. It treats categories as ordered
(by their position in the table) and asks: do adjacent categories
exchange members more often than distant ones?

``` r
m_ua <- fit_mobility_model(mx, type = "uniform_association")
print(m_ua)
#> Log-Linear Model for Mobility Table
#> ====================================
#> Type: uniform_association
#> Categories: 8
#> 
#> Deviance (G2): 3491.285
#> Df: 48
#> P-value: 0
#> Dissimilarity index: 0.220
#> AIC: 3885.943
#> BIC: 3920.485
```

This model is useful when categories have a natural ordering (e.g.,
social classes from lower to upper). If the mobility structure is
multidimensional rather than ordinal, the single association parameter
will not capture it well.

### MONECA topology

The MONECA topology model uses segment assignments from a MONECA
analysis as structural predictors. It adds diagonal parameters (like
quasi-independence) plus a segment-pair factor that distinguishes
within-segment from between-segment flows.

``` r
m_top <- fit_mobility_model(seg, type = "moneca_topology", level = 2)
print(m_top)
#> Log-Linear Model for Mobility Table
#> ====================================
#> Type: moneca_topology
#> Categories: 8
#> Segment level: 2
#> 
#> Deviance (G2): 1290.816
#> Df: 38
#> P-value: 3.06e-246
#> Dissimilarity index: 0.087
#> AIC: 1705.474
#> BIC: 1761.605
```

If this model fits substantially better than quasi-independence, the
segments identified by MONECA correspond to genuine mobility barriers:
people move within segments more than expected and between segments less
than expected.

## Reading the output

Each
[`fit_mobility_model()`](https://gmontaletti.github.io/MONECA/reference/fit_mobility_model.md)
call returns a `moneca_loglinear` object with several fields:

- **Deviance (G²)**: the likelihood-ratio statistic measuring how far
  the model is from the saturated model (which fits the data exactly).
  Lower values indicate better fit.
- **Degrees of freedom (df)**: the number of free data points minus the
  number of model parameters. More complex models use more parameters
  and have fewer degrees of freedom.
- **P-value**: derived from a chi-squared test of the deviance. A large
  p-value (\> 0.05) means the model fits adequately; a small p-value
  means the model misses structure present in the data.
- **Dissimilarity index**: the proportion of cases that would need to be
  reclassified for the model to reproduce the observed table exactly.
  Values below 0.05 indicate good fit; below 0.03, excellent fit.
- **AIC / BIC**: information criteria for comparing non-nested models.
  Lower values are preferred. BIC penalizes complexity more heavily than
  AIC.
- **Fitted values** (`$fitted`): the model’s predicted cell counts, as
  an n x n matrix.
- **Residuals** (`$residuals`): Pearson residuals, as an n x n matrix.
  Large positive residuals indicate flows the model under-predicts.

## Comparing models

The
[`compare_mobility_models()`](https://gmontaletti.github.io/MONECA/reference/compare_mobility_models.md)
function produces a summary table and likelihood-ratio tests in a single
call. When passed a moneca object via `mx`, it automatically fits all
five model types:

``` r
comp <- compare_mobility_models(mx = seg, level = 2)
print(comp)
#> Log-Linear Model Comparison
#> ===========================
#> 
#>                model  deviance df       aic       bic   p_value dissimilarity
#>       quasi_symmetry    19.811 21   468.469   561.301  5.33e-01         0.008
#>      moneca_topology  1290.816 38  1705.474  1761.605 3.06e-246         0.087
#>  uniform_association  3491.285 48  3885.943  3920.485  0.00e+00         0.220
#>   quasi_independence  3843.491 41  4252.149  4301.803  0.00e+00         0.176
#>         independence 14676.049 49 15068.707 15101.091  0.00e+00         0.500
#> 
#> Likelihood-Ratio Tests:
#>             model_1             model_2 delta_deviance delta_df p_value
#>        independence  quasi_independence      10832.559        8       0
#>        independence uniform_association      11184.765        1       0
#>  quasi_independence      quasi_symmetry       3823.680       20       0
#>  quasi_independence     moneca_topology       2552.674        3       0
```

### Reading the comparison table

The table is sorted by BIC (lowest first). Key columns to examine:

- **BIC**: the primary criterion for model selection among non-nested
  models. The model with the lowest BIC provides the best trade-off
  between fit and parsimony.
- **Dissimilarity**: a more intuitive measure of practical fit. Compare
  values across models to see which ones capture the data most
  accurately.
- **P-value**: if any model has a large p-value, it fits the data
  adequately on its own. If all p-values are small, all models miss some
  structure, and the comparison focuses on which misses least.

### Reading the likelihood-ratio tests

The `lr_tests` component tests whether moving from a simpler model to a
more complex one provides a statistically significant improvement. Each
row shows:

- **model_1** (simpler) and **model_2** (more complex): the two nested
  models
- **delta_deviance**: the reduction in G² when moving from the simpler
  to the more complex model
- **delta_df**: the number of additional parameters used
- **p_value**: if small, the more complex model captures real structure
  that the simpler one misses

For example, a significant test between quasi-independence and MONECA
topology means that the MONECA segments explain mobility patterns beyond
what diagonal immobility alone can account for. The segments correspond
to genuine structural features of the table, not artifacts of the
clustering algorithm.

## Working with larger matrices

The log-linear functions scale to large matrices. The quasi-symmetry
model uses IPF rather than GLM, so it remains fast even when GLM-based
fitting becomes numerically expensive.

``` r
mx_large <- generate_mobility_data(
  n_classes = 30,
  n_total = 50000,
  immobility_strength = 0.6,
  class_clustering = 0.3,
  seed = 99
)

seg_large <- moneca_fast(mx_large, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |======================================================================| 100%

# Fit all models and compare
comp_large <- compare_mobility_models(mx = seg_large, level = 2)
print(comp_large)
#> Log-Linear Model Comparison
#> ===========================
#> 
#>                model  deviance  df        aic        bic p_value dissimilarity
#>       quasi_symmetry    120.32 406   2269.943   4642.326       1         0.008
#>      moneca_topology  13993.01 746  15462.638  16202.207       0         0.113
#>  uniform_association  26763.13 840  28044.757  28332.900       0         0.246
#>   quasi_independence  74820.91 811  76160.538  76587.951       0         0.345
#>         independence 190407.61 841 191687.230 191970.571       0         0.813
#> 
#> Likelihood-Ratio Tests:
#>             model_1             model_2 delta_deviance delta_df p_value
#>        independence  quasi_independence       115586.7       30       0
#>        independence uniform_association       163644.5        1       0
#>  quasi_independence      quasi_symmetry        74700.6      405       0
#>  quasi_independence     moneca_topology        60827.9       65       0
```

For matrices with 50 or more categories, the quasi-symmetry model fitted
via IPF typically converges in 20–50 iterations, while a GLM-based
approach for the same model would require thousands of symmetric-pair
parameters.

## Inspecting residuals

The residuals from the best-fitting model identify specific flows that
the model does not capture well. Large positive Pearson residuals point
to transitions that occur more often than the model predicts —
unexpected mobility channels that may warrant further investigation.

``` r
# Extract residuals from the MONECA topology model
resid_mx <- m_top$residuals

# Convert to a sorted data frame
resid_df <- data.frame(
  Origin = rep(rownames(resid_mx), ncol(resid_mx)),
  Destination = rep(colnames(resid_mx), each = nrow(resid_mx)),
  Residual = as.vector(resid_mx),
  stringsAsFactors = FALSE
)

# Remove diagonal (immobility is accounted for by the model)
resid_df <- resid_df[resid_df$Origin != resid_df$Destination, ]

# Show the largest under-predicted flows
resid_df <- resid_df[order(-resid_df$Residual), ]
head(resid_df, 10)
#>     Origin Destination  Residual
#> 45 Class 5     Class 6 15.425074
#> 39 Class 7     Class 5 11.555896
#> 17 Class 1     Class 3 10.465040
#> 38 Class 6     Class 5  9.117058
#> 11 Class 3     Class 2  7.256005
#> 30 Class 6     Class 4  5.509197
#> 18 Class 2     Class 3  5.506344
#> 3  Class 3     Class 1  5.153430
#> 29 Class 5     Class 4  5.145182
#> 56 Class 8     Class 7  4.498055
```

These are category pairs where observed mobility exceeds the model’s
prediction. They represent flows that cross the segment boundaries
identified by MONECA — transitions between clusters that the topology
model does not anticipate. Such cross-segment flows can reveal labor
market connections, institutional pathways, or structural changes that a
cluster-based model misses.

## Summary

The typical workflow for log-linear analysis of a mobility table is:

1.  **Run MONECA**
    ([`moneca()`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
    or
    [`moneca_fast()`](https://gmontaletti.github.io/MONECA/reference/moneca_fast.md))
    to identify segment structure
2.  **Compare models** with
    [`compare_mobility_models()`](https://gmontaletti.github.io/MONECA/reference/compare_mobility_models.md)
    to assess whether the segments capture genuine patterns
3.  **Inspect residuals** from the best model to identify unexpected
    mobility channels

The five model types test different hypotheses:

| Model               | What it tests                                  | Typical use                    |
|:--------------------|:-----------------------------------------------|:-------------------------------|
| Independence        | No association between origin and destination  | Baseline                       |
| Quasi-independence  | Immobility only; off-diagonal flows are random | Standard mobility baseline     |
| Quasi-symmetry      | Flows are balanced in both directions          | Test for directional asymmetry |
| Uniform association | Single ordinal gradient drives mobility        | Test for hierarchical ordering |
| MONECA topology     | Segment structure explains mobility barriers   | Validate MONECA segmentation   |

When the MONECA topology model outperforms quasi-independence, it
confirms that the algorithmic segmentation identifies real structural
boundaries in the mobility data. The segments are not just a convenient
visualization — they correspond to statistically measurable
discontinuities in how people move between categories.
