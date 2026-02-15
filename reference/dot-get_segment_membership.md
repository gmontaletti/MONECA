# Extract segment membership from a moneca object

Returns a named character vector mapping category names to segment
labels at a given hierarchical level.

## Usage

``` r
.get_segment_membership(moneca_obj, level)
```

## Arguments

- moneca_obj:

  A moneca object.

- level:

  Integer specifying the hierarchical level.

## Value

A named character vector where names are category names and values are
segment labels (e.g., `"seg_1"`).
