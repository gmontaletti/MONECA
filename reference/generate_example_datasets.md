# Generate Predefined Example Datasets

Creates a collection of predefined mobility datasets with different
structural characteristics. These examples are useful for tutorials,
testing different moneca parameters, and demonstrating various mobility
regime types.

## Usage

``` r
generate_example_datasets()
```

## Value

A named list containing mobility matrices with different
characteristics:

- simple:

  Small 4-class example with clear segmentation

- complex:

  Larger 8-class example with hierarchical structure

- rigid:

  High immobility example showing strong class boundaries

- fluid:

  Low immobility example with extensive mobility

- polarized:

  Example with strong upper/lower class divide

- gradual:

  Example with smooth class transitions

Each matrix is in standard moneca format with row/column totals.

## Details

This function provides ready-to-use examples that demonstrate different
types of social mobility structures:

- **Simple**: Good for learning moneca basics

- **Complex**: Tests algorithm performance on larger structures

- **Rigid**: Shows segmentation in low-mobility societies

- **Fluid**: Tests segmentation in high-mobility contexts

- **Polarized**: Demonstrates class divide patterns

- **Gradual**: Shows continuous mobility gradients

These examples are particularly useful for:

- Tutorial and teaching materials

- Comparing moneca parameters across mobility types

- Testing visualization functions

- Benchmarking algorithm performance

## See also

[`generate_mobility_data`](https://gmontaletti.github.io/MONECA/reference/generate_mobility_data.md)
for custom synthetic data,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
analysis,
[`plot_moneca_ggraph`](https://gmontaletti.github.io/MONECA/reference/plot_moneca_ggraph.md)
for visualization

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all example datasets
examples <- generate_example_datasets()
names(examples)

# Examine the simple example
print(examples$simple)

# Compare different mobility regimes
rigid_seg <- moneca(examples$rigid, segment.levels = 3)
fluid_seg <- moneca(examples$fluid, segment.levels = 3)

# Visualize different structures
plot_moneca_ggraph(rigid_seg, title = "Rigid Class Structure")
plot_moneca_ggraph(fluid_seg, title = "Fluid Mobility Regime")
} # }
```
