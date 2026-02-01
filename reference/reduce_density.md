# Reduce Density of Large Mobility Matrices

Preprocesses large mobility matrices to reduce noise through
dimensionality reduction techniques. This function is designed for
matrices with 60+ categories where noise can obscure meaningful mobility
patterns.

## Usage

``` r
reduce_density(
  mx,
  method = c("svd", "nmf"),
  normalization = c("none", "pearson", "ppmi"),
  k = "auto",
  variance_target = 0.75,
  threshold = NULL,
  threshold_type = c("sd", "percentile"),
  verbose = FALSE,
  seed = NULL,
  filter_quantile = "auto"
)
```

## Arguments

- mx:

  A mobility matrix with totals row/column in standard moneca format.
  Must have at least 4 rows/columns (excluding totals).

- method:

  Character string specifying the dimensionality reduction method:

  - `"svd"` (default): Singular Value Decomposition - fast and
    memory-efficient, produces continuous values

  - `"nmf"`: Non-negative Matrix Factorization - preserves
    non-negativity, better interpretability for count data

- normalization:

  Character string specifying pre-processing normalization:

  - `"none"` (default): No normalization, use raw counts

  - `"pearson"`: Pearson residuals - standardizes for marginal effects

  - `"ppmi"`: Positive Pointwise Mutual Information - emphasizes
    mobility above chance levels

- k:

  Number of components to retain. Either `"auto"` (default) for
  automatic selection based on variance explained, or a positive
  integer. Typical range for 60+ category matrices is 15-25.

- variance_target:

  Numeric value (0-1) specifying target cumulative variance to explain
  when `k = "auto"`. Default is 0.75. Higher values retain more detail
  but also more noise.

- threshold:

  Optional sparsification threshold. Can be:

  - `NULL` (default): No thresholding

  - `"auto"`: Automatic threshold (mean + 1 SD)

  - Numeric: Values below this threshold are set to zero

- threshold_type:

  Character string specifying how numeric threshold is interpreted:

  - `"sd"` (default): Number of standard deviations above mean

  - `"percentile"`: Percentile threshold (0-100)

- verbose:

  Logical indicating whether to print progress information. Default is
  `FALSE`.

- seed:

  Optional integer for reproducibility. Particularly important for NMF
  which uses random initialization.

- filter_quantile:

  Controls the quantile threshold for filtering. Applies to both SVD and
  NMF methods. Can be:

  - `"auto"` (default): Automatically selects threshold via elbow
    detection on reconstruction values

  - Numeric (0-1): Keep cells with reconstruction values above this
    quantile. E.g., 0.75 keeps top 75

  Both SVD and NMF use reconstruction values as a filter to identify
  significant cells, then return the original values for those cells.
  This preserves count interpretability while reducing network density.

## Value

An object of class `"density_reduced"` (inherits from matrix) with the
following attributes:

- method:

  The reduction method used ("svd" or "nmf")

- normalization:

  The normalization method used

- k:

  Number of components retained

- variance_explained:

  Proportion of variance explained (SVD only)

- filter_quantile:

  The filter quantile used to select cells

- threshold_applied:

  The threshold value applied (if any)

- original_dims:

  Dimensions of the original core matrix

## Details

The function implements a pipeline for reducing noise in large mobility
matrices:

1.  **Input Validation**: Checks matrix format and extracts core matrix
    (excluding totals row/column)

2.  **Normalization**: Optionally transforms counts using PPMI or
    Pearson residuals

3.  **Component Selection**: When `k = "auto"`, uses variance explained
    with elbow detection

4.  **Dimensionality Reduction**: Applies SVD or NMF to extract dominant
    patterns

5.  **Reconstruction**: Reconstructs matrix from reduced components

6.  **Filtering**: Uses reconstruction values as a filter to identify
    significant cells. Cells with reconstruction values below the
    threshold (based on `filter_quantile`) are set to zero. Original
    cell values are preserved for cells that pass the filter.

7.  **Thresholding**: Optionally applies additional thresholding

8.  **Totals**: Recalculates row/column totals

For SVD, the reconstruction is \\M \approx U_k \Sigma_k V_k^T\\ where k
components are retained. For NMF, the factorization is \\M \approx W H\\
with non-negative factors. Both methods use reconstruction values to
filter cells while preserving original counts for retained cells.

## Dependencies

- `irlba`: Optional, for fast truncated SVD. Falls back to base R
  [`svd()`](https://rdrr.io/r/base/svd.html) if not available.

- `RcppML`: Required only for `method = "nmf"`

## See also

[`plot_scree`](https://gmontaletti.github.io/MONECA/reference/plot_scree.md)
for visualizing component selection,
[`weight.matrix`](https://gmontaletti.github.io/MONECA/reference/weight.matrix.md)
for the next step in the pipeline,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function

## Examples

``` r
# Generate large synthetic data
large_data <- generate_mobility_data(n_classes = 60, n_total = 30000, seed = 123)

# Basic SVD reduction with auto k selection
reduced <- reduce_density(large_data, verbose = TRUE)
#> Input matrix: 61 x 61 (core: 60 x 60)
#> Total observations: 30000
#> Computing singular values for automatic k selection...
#> Warning: You're computing too large a percentage of total singular values, use a standard svd instead.
#>   Variance target (75%): k = 12
#>   Elbow detection: k = 10
#>   Selected k = 12 (76.2% variance explained)
#> Using k = 12 components
#> Performing SVD with k = 12 components...
#> Auto quantile selection: keep top 25.0% of cells (elbow at 269/3006)
#> SVD filter: keeping 752/3006 cells (25.0%) with reconstruction >= 2.4375
#> Reduction complete: 30000 -> 24784 observations (82.6% retained)
print(reduced)
#> Density-Reduced Mobility Matrix
#> ================================
#> Dimensions: 60 x 60 categories
#> Method: svd
#> Normalization: none
#> Components (k): 12
#> Variance explained: 76.2%
#> Filter quantile: 25.0% (keeps top cells by reconstruction)
#> Observations: 30000 -> 24784 (82.6% retained)
#> 
#> Core matrix preview (first 5x5):
#>         Class 1 Class 2 Class 3 Class 4 Class 5
#> Class 1     208      45      23       1       0
#> Class 2     139     581     167      69       0
#> Class 3       1       5      26       5       3
#> Class 4       0      31      79     270      57
#> Class 5       0       0      90     161     702

# SVD with PPMI normalization
reduced_ppmi <- reduce_density(large_data,
                               normalization = "ppmi",
                               variance_target = 0.80,
                               verbose = TRUE)
#> Input matrix: 61 x 61 (core: 60 x 60)
#> Total observations: 30000
#> Applied ppmi normalization
#> Computing singular values for automatic k selection...
#> Warning: You're computing too large a percentage of total singular values, use a standard svd instead.
#>   Variance target (80%): k = 14
#>   Elbow detection: k = 13
#>   Selected k = 14 (82.9% variance explained)
#> Using k = 14 components
#> Performing SVD with k = 14 components...
#> Auto quantile selection: keep top 25.0% of cells (elbow at 437/1989)
#> SVD filter: keeping 498/1989 cells (25.0%) with reconstruction >= 0.3741
#> Reduction complete: 30000 -> 27821 observations (92.7% retained)

# NMF reduction with fixed k
if (FALSE) { # \dontrun{
reduced_nmf <- reduce_density(large_data,
                              method = "nmf",
                              k = 20,
                              seed = 42)
} # }

# With thresholding to increase sparsity
reduced_sparse <- reduce_density(large_data,
                                 threshold = 1,
                                 threshold_type = "sd")
#> Warning: You're computing too large a percentage of total singular values, use a standard svd instead.

# Continue with standard moneca pipeline
seg <- moneca(reduced, segment.levels = 3)
#>   |                                                                              |                                                                      |   0%  |                                                                              |                                                                      |   1%  |                                                                              |=                                                                     |   1%  |                                                                              |=                                                                     |   2%  |                                                                              |==                                                                    |   3%  |                                                                              |===                                                                   |   4%  |                                                                              |===                                                                   |   5%  |                                                                              |====                                                                  |   5%  |                                                                              |====                                                                  |   6%  |                                                                              |=====                                                                 |   6%  |                                                                              |=====                                                                 |   7%  |                                                                              |=====                                                                 |   8%  |                                                                              |======                                                                |   8%  |                                                                              |======                                                                |   9%  |                                                                              |=======                                                               |  10%  |                                                                              |========                                                              |  11%  |                                                                              |========                                                              |  12%  |                                                                              |=========                                                             |  12%  |                                                                              |=========                                                             |  13%  |                                                                              |==========                                                            |  14%  |                                                                              |==========                                                            |  15%  |                                                                              |===========                                                           |  16%  |                                                                              |============                                                          |  17%  |                                                                              |============                                                          |  18%  |                                                                              |=============                                                         |  18%  |                                                                              |=============                                                         |  19%  |                                                                              |==============                                                        |  19%  |                                                                              |==============                                                        |  20%  |                                                                              |===============                                                       |  21%  |                                                                              |===============                                                       |  22%  |                                                                              |================                                                      |  23%  |                                                                              |=================                                                     |  24%
#> 25% ready!
#>   |                                                                              |=================                                                     |  25%  |                                                                              |==================                                                    |  25%  |                                                                              |==================                                                    |  26%  |                                                                              |===================                                                   |  27%  |                                                                              |====================                                                  |  28%  |                                                                              |====================                                                  |  29%  |                                                                              |=====================                                                 |  30%  |                                                                              |=====================                                                 |  31%  |                                                                              |======================                                                |  31%  |                                                                              |======================                                                |  32%  |                                                                              |=======================                                               |  32%  |                                                                              |=======================                                               |  33%  |                                                                              |========================                                              |  34%  |                                                                              |=========================                                             |  35%  |                                                                              |=========================                                             |  36%  |                                                                              |==========================                                            |  37%  |                                                                              |==========================                                            |  38%  |                                                                              |===========================                                           |  38%  |                                                                              |===========================                                           |  39%  |                                                                              |============================                                          |  40%  |                                                                              |=============================                                         |  41%  |                                                                              |=============================                                         |  42%  |                                                                              |==============================                                        |  42%  |                                                                              |==============================                                        |  43%  |                                                                              |==============================                                        |  44%  |                                                                              |===============================                                       |  44%  |                                                                              |===============================                                       |  45%  |                                                                              |================================                                      |  45%  |                                                                              |================================                                      |  46%  |                                                                              |=================================                                     |  47%  |                                                                              |==================================                                    |  48%  |                                                                              |==================================                                    |  49%  |                                                                              |===================================                                   |  49%
#> 50% ready!
#>   |                                                                              |===================================                                   |  50%  |                                                                              |===================================                                   |  51%  |                                                                              |====================================                                  |  51%  |                                                                              |====================================                                  |  52%  |                                                                              |=====================================                                 |  53%  |                                                                              |======================================                                |  54%  |                                                                              |======================================                                |  55%  |                                                                              |=======================================                               |  55%  |                                                                              |=======================================                               |  56%  |                                                                              |========================================                              |  56%  |                                                                              |========================================                              |  57%  |                                                                              |========================================                              |  58%  |                                                                              |=========================================                             |  58%  |                                                                              |=========================================                             |  59%  |                                                                              |==========================================                            |  60%  |                                                                              |===========================================                           |  61%  |                                                                              |===========================================                           |  62%  |                                                                              |============================================                          |  62%  |                                                                              |============================================                          |  63%  |                                                                              |=============================================                         |  64%  |                                                                              |=============================================                         |  65%  |                                                                              |==============================================                        |  66%  |                                                                              |===============================================                       |  67%  |                                                                              |===============================================                       |  68%  |                                                                              |================================================                      |  68%  |                                                                              |================================================                      |  69%  |                                                                              |=================================================                     |  69%  |                                                                              |=================================================                     |  70%  |                                                                              |==================================================                    |  71%  |                                                                              |==================================================                    |  72%  |                                                                              |===================================================                   |  73%  |                                                                              |====================================================                  |  74%  |                                                                              |====================================================                  |  75%
#> 75% ready!
#>   |                                                                              |=====================================================                 |  75%  |                                                                              |=====================================================                 |  76%  |                                                                              |======================================================                |  77%  |                                                                              |=======================================================               |  78%  |                                                                              |=======================================================               |  79%  |                                                                              |========================================================              |  80%  |                                                                              |========================================================              |  81%  |                                                                              |=========================================================             |  81%  |                                                                              |=========================================================             |  82%  |                                                                              |==========================================================            |  82%  |                                                                              |==========================================================            |  83%  |                                                                              |===========================================================           |  84%  |                                                                              |============================================================          |  85%  |                                                                              |============================================================          |  86%  |                                                                              |=============================================================         |  87%  |                                                                              |=============================================================         |  88%  |                                                                              |==============================================================        |  88%  |                                                                              |==============================================================        |  89%  |                                                                              |===============================================================       |  90%  |                                                                              |================================================================      |  91%  |                                                                              |================================================================      |  92%  |                                                                              |=================================================================     |  92%  |                                                                              |=================================================================     |  93%  |                                                                              |=================================================================     |  94%  |                                                                              |==================================================================    |  94%  |                                                                              |==================================================================    |  95%  |                                                                              |===================================================================   |  95%  |                                                                              |===================================================================   |  96%  |                                                                              |====================================================================  |  97%  |                                                                              |===================================================================== |  98%  |                                                                              |===================================================================== |  99%  |                                                                              |======================================================================|  99%  |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   6%  |                                                                              |========                                                              |  11%  |                                                                              |============                                                          |  17%  |                                                                              |================                                                      |  22%
#> 26% ready!
#>   |                                                                              |===================                                                   |  28%  |                                                                              |=======================                                               |  33%  |                                                                              |===========================                                           |  39%  |                                                                              |===============================                                       |  44%  |                                                                              |===================================                                   |  50%
#> 53% ready!
#>   |                                                                              |=======================================                               |  56%  |                                                                              |===========================================                           |  61%  |                                                                              |===============================================                       |  67%  |                                                                              |===================================================                   |  72%  |                                                                              |======================================================                |  78%
#> 79% ready!
#>   |                                                                              |==========================================================            |  83%  |                                                                              |==============================================================        |  89%  |                                                                              |==================================================================    |  94%  |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
#>   |                                                                              |                                                                      |   0%  |                                                                              |==============                                                        |  20%
#> 33% ready!
#>   |                                                                              |============================                                          |  40%
#> 50% ready!
#>   |                                                                              |==========================================                            |  60%  |                                                                              |========================================================              |  80%
#> 83% ready!
#>   |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
```
