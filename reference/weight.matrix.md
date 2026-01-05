# Calculate Relative Risk Weight Matrix

Converts a mobility table into a relative risk matrix by comparing
observed mobility flows to expected flows under independence. This
matrix forms the basis for network construction in moneca analysis.

## Usage

``` r
weight.matrix(
  mx,
  cut.off = 1,
  symmetric = TRUE,
  diagonal = NULL,
  small.cell.reduction = 0,
  auto_tune = FALSE,
  tune_method = "stability",
  tune_verbose = FALSE
)
```

## Arguments

- mx:

  A mobility table (square matrix) with row and column totals in the
  last row/column.

- cut.off:

  Numeric threshold for minimum relative risk. Values below this are set
  to NA. Default is 1.

- symmetric:

  Logical indicating whether to force the matrix to be symmetric by
  adding it to its transpose. Default is TRUE.

- diagonal:

  Controls diagonal values. If NULL (default), diagonal is set to NA.
  Otherwise, diagonal values are preserved.

- small.cell.reduction:

  Numeric value for handling small cells. Cells with counts below this
  threshold are set to 0 before calculating relative risks. Default is
  0.

- auto_tune:

  Logical indicating whether to automatically tune the
  small.cell.reduction parameter. When TRUE, uses optimization methods
  to select the best value. Default is FALSE.

- tune_method:

  Character string specifying the auto-tuning method when auto_tune is
  TRUE. Options are "stability" (default), "quality", or "performance".
  See
  [`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)
  for details.

- tune_verbose:

  Logical indicating whether to print verbose messages during
  auto-tuning. Default is FALSE.

## Value

A matrix of relative risks where:

- Values \> 1 indicate mobility above expected levels

- Values \< 1 indicate mobility below expected levels

- Values below cut.off are set to NA

When auto_tune is TRUE, the matrix includes additional attributes:

- `auto_tuning`: Complete results from auto-tuning process

- `auto_tuned`: Logical flag indicating auto-tuning was used

- `tuned_small_cell_reduction`: The selected optimal value

## Details

The relative risk for cell (i,j) is calculated as: \$\$RR\_{ij} =
O\_{ij} / E\_{ij}\$\$ where \\O\_{ij}\\ is the observed count and
\\E\_{ij}\\ is the expected count under independence: \\E\_{ij} = (n_i
\* n_j) / N\\

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function,
[`auto_tune_small_cell_reduction`](https://gmontaletti.github.io/MONECA/reference/auto_tune_small_cell_reduction.md)
for auto-tuning details

## Examples

``` r
# Create a simple mobility table
mob_table <- matrix(c(100, 20, 10, 130,
                      15, 80, 25, 120,  
                      5,  10, 50,  65,
                      120, 110, 85, 315), 
                    nrow = 4, byrow = TRUE)
rownames(mob_table) <- colnames(mob_table) <- c("A", "B", "C", "Total")

# Calculate relative risk matrix
rr_matrix <- weight.matrix(mob_table, cut.off = 1.5)

# Use automatic tuning for small.cell.reduction parameter
rr_matrix_tuned <- weight.matrix(mob_table, auto_tune = TRUE, 
                                 tune_method = "stability", tune_verbose = TRUE)
#> Auto-tuning small.cell.reduction parameter using method: stability
#> Parallel decision: NO ( 1 cores)
#> Starting auto-tuning for small.cell.reduction parameter...
#> Method: stability 
#> Matrix size: 4 x 4 
#> Parallel processing: NO 
#> Generated 20 candidate values
#> 
#> Evaluating 20 candidates using optimized batch processing...
#>   |                                                                              |                                                                      |   0%  |                                                                              |====                                                                  |   5%  |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |=======                                                               |  10%  |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |==========                                                            |  15%  |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |==============                                                        |  20%  |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |==================                                                    |  25%  |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |                                                                      |   0%
#>   |                                                                              |=====================                                                 |  30%  |                                                                              |========================                                              |  35%  |                                                                              |============================                                          |  40%  |                                                                              |================================                                      |  45%  |                                                                              |===================================                                   |  50%  |                                                                              |======================================                                |  55%  |                                                                              |==========================================                            |  60%  |                                                                              |==============================================                        |  65%  |                                                                              |=================================================                     |  70%  |                                                                              |====================================================                  |  75%  |                                                                              |========================================================              |  80%  |                                                                              |============================================================          |  85%  |                                                                              |===============================================================       |  90%  |                                                                              |==================================================================    |  95%  |                                                                              |======================================================================| 100%
#> 
#> 
#> Batch evaluation completed
#> - Valid results: 5 of 20 
#> - Mean stability score: 0.25 
#> - Mean quality score: 0.154 
#> - Weight matrices cached: 20 
#> 
#> Optimal small.cell.reduction: 0 
#> Total tuning time: 8.01 seconds
#> Selected optimal small.cell.reduction: 0

# Check tuning results
if (attr(rr_matrix_tuned, "auto_tuned", exact = TRUE)) {
  optimal_value <- attr(rr_matrix_tuned, "tuned_small_cell_reduction", exact = TRUE)
  message("Optimal small.cell.reduction: ", optimal_value)
}
#> Optimal small.cell.reduction: 0
```
