# Generate Realistic Synthetic Mobility Data

Creates synthetic social mobility tables with configurable patterns that
mimic real-world mobility structures. This function is essential for
testing moneca algorithms, creating examples, and understanding how
different mobility patterns affect segmentation results.

## Usage

``` r
generate_mobility_data(
  n_classes = 10,
  n_total = 10000,
  immobility_strength = 0.6,
  class_clustering = 0.3,
  noise_level = 0.1,
  class_names = NULL,
  seed = NULL
)
```

## Arguments

- n_classes:

  Integer specifying the number of social classes or occupational
  categories. Must be at least 2. Default is 10.

- n_total:

  Integer specifying the total sample size (number of individuals). Must
  be at least as large as n_classes. Default is 10000.

- immobility_strength:

  Numeric value (0-1) controlling the strength of diagonal immobility
  (tendency to remain in origin class). Higher values create more rigid
  class structures. Default is 0.6.

- class_clustering:

  Numeric value (0-1) controlling the tendency for mobility between
  adjacent classes in the social hierarchy. Higher values create more
  gradual, short-distance mobility. Default is 0.3.

- noise_level:

  Numeric value (0-1) controlling the amount of random mobility between
  non-adjacent classes. Higher values create more chaotic mobility
  patterns. Default is 0.1.

- class_names:

  Character vector of names for the social classes. If NULL, classes are
  named "Class 1", "Class 2", etc. Length must equal n_classes.

- seed:

  Integer for random seed to ensure reproducible results. Default is
  NULL (no seed set).

## Value

A square matrix in moneca format with:

- Core matrix:

  Upper-left (n_classes x n_classes) contains mobility flows

- Row margins:

  Last column contains origin class totals

- Column margins:

  Last row contains destination class totals

- Grand total:

  Bottom-right cell contains total sample size

The matrix includes row and column names for easy interpretation.

## Details

This function generates mobility data using a hierarchical model that
reflects common patterns in social mobility research:

1.  **Diagonal Dominance**: Most people remain in their origin class

2.  **Distance Effects**: Mobility is more likely between adjacent
    classes

3.  **Marginal Heterogeneity**: Class sizes vary realistically

4.  **Random Component**: Some unpredictable mobility occurs

The generated data can be used to test how different mobility regimes
affect moneca segmentation and to create realistic examples for
demonstrations.

Parameter combinations:

- High immobility + low clustering = rigid class boundaries

- Low immobility + high clustering = fluid but structured mobility

- High noise = chaotic mobility patterns

- Balanced parameters = realistic social mobility

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
analyzing the generated data,
[`generate_example_datasets`](https://gmontaletti.github.io/MONECA/reference/generate_example_datasets.md)
for predefined examples,
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for visualizing results

## Examples

``` r
# Basic synthetic data with default parameters
basic_data <- generate_mobility_data()
print(basic_data[1:6, 1:6])  # Show first 5 classes plus totals
#>         Class 1 Class 2 Class 3 Class 4 Class 5 Class 6
#> Class 1     255      64      42       7       4       4
#> Class 2     194     703     179     104      11       7
#> Class 3     135     276    1046     280     156      17
#> Class 4       2      25      51     141      40      18
#> Class 5       4       3      28      58     207      60
#> Class 6       5       7       1      23      71     251

# Small example for quick testing
test_data <- generate_mobility_data(
  n_classes = 5,
  n_total = 1000,
  immobility_strength = 0.7,
  class_clustering = 0.2,
  seed = 42
)

# Professional class structure with custom names
professional_data <- generate_mobility_data(
  n_classes = 6,
  n_total = 5000,
  class_names = c("Upper", "Upper-Middle", "Middle", 
                  "Lower-Middle", "Working", "Lower"),
  immobility_strength = 0.5,
  class_clustering = 0.4,
  seed = 123
)

# Highly fluid mobility regime
fluid_data <- generate_mobility_data(
  n_classes = 8,
  immobility_strength = 0.3,
  class_clustering = 0.1,
  noise_level = 0.3
)

# Use in moneca analysis
seg <- moneca(test_data, segment.levels = 3)
#>   |                                                                              |                                                                      |   0%
#> 33% ready!
#>   |                                                                              |===================================                                   |  50%
#> 67% ready!
#>   |                                                                              |======================================================================| 100%
#> 100% ready!
#> 
if (FALSE) { # \dontrun{
plot_moneca_ggraph(seg, title = "Synthetic Mobility Analysis")
} # }
```
