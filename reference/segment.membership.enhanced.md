# Enhanced Segment Membership with Meaningful Names

Returns segment membership information with an additional column
containing meaningful segment names derived from the constituent nodes.
This function extends
[`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
by adding intelligent naming strategies for aggregated segments.

## Usage

``` r
segment.membership.enhanced(
  segments,
  level = seq(segments$segment.list),
  naming_strategy = "auto",
  custom_names = NULL,
  separator = " + ",
  max_concat_length = 2,
  pattern_rules = NULL
)
```

## Arguments

- segments:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md).

- level:

  Integer vector specifying which hierarchical levels to include.
  Default includes all available levels.

- naming_strategy:

  Character string specifying the naming approach:

  - "auto" (default): Automatic naming based on segment composition

  - "concat": Concatenate node names with separator

  - "pattern": Use pattern recognition for common suffixes/prefixes

  - "custom": Use user-provided names from custom_names parameter

- custom_names:

  Named list providing custom names for specific segments. Format:
  list("level.segment" = "Custom Name"), e.g., list("2.1" = "Blue
  Collar")

- separator:

  Character string used to separate names when concatenating. Default is
  " + ".

- max_concat_length:

  Maximum number of names to concatenate before switching to
  pattern-based naming. Default is 2.

- pattern_rules:

  Enhanced pattern rules list with priorities and semantic categories.
  Default includes comprehensive occupational and industry patterns
  (avoiding generic social class terms).

## Value

A data frame with three columns:

- name:

  Character vector of original category names

- membership:

  Character vector indicating segment membership, formatted as
  "level.segment" (e.g., "2.1" for level 2, segment 1)

- segment_label:

  Character vector with meaningful segment names derived from
  constituent nodes or user-provided names

## Details

The function applies different naming strategies based on segment
composition:

1.  **Individual nodes**: Keep original name unchanged

2.  **Small segments** (\<= max_concat_length): Concatenate names

3.  **Large segments**: Apply pattern recognition or use generic names

4.  **Custom names**: Override automatic naming with user-provided names

The enhanced pattern recognition system uses weighted scoring to
identify occupational hierarchies, industry sectors, and employment
types. It creates semantically meaningful names like
"Executive_Leadership", "Professional_Services", or
"Technical_Specialists" based on segment composition, avoiding generic
terms.

## See also

[`segment.membership`](https://gmontaletti.github.io/MONECA/reference/segment.membership.md)
for basic membership information,
[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate data and run analysis
mob_data <- generate_mobility_data(n_classes = 6, seed = 42)
seg <- moneca(mob_data, segment.levels = 3)

# Get enhanced membership with automatic naming
enhanced <- segment.membership.enhanced(seg)
print(enhanced)
} # }
```
