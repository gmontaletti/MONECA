# Print Method for Descriptive MONECA Statistics

Internal function that formats and displays the detailed statistics
calculated by the `print.moneca` method. This function creates formatted
tables showing network properties and mobility statistics across
hierarchical levels.

## Usage

``` r
# S3 method for class 'descriptive.moneca'
print(x, ...)
```

## Arguments

- x:

  A descriptive.moneca object containing calculated statistics.

- ...:

  Additional arguments (currently unused).

## Value

NULL (called for side effects - printing formatted output).

## Details

This internal function is responsible for the formatted display of
MONECA analysis results. It creates several summary tables:

- Degree distributions (all, in, out) for each level

- Edge weight distributions for each level

- Diagonal mobility percentages

- Network density and connectivity statistics
