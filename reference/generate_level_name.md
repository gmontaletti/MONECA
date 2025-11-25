# Generate Level Name Based on Strategy

Internal helper function to generate meaningful level names for
segments.

## Usage

``` r
generate_level_name(
  node_names,
  membership_id,
  naming_strategy,
  custom_names,
  separator,
  max_concat_length,
  pattern_rules
)
```

## Arguments

- node_names:

  Character vector of node names in the segment

- membership_id:

  Character string of the membership ID (e.g., "2.1")

- naming_strategy:

  Character string specifying naming approach

- custom_names:

  Named list of custom names

- separator:

  Character string for concatenation

- max_concat_length:

  Maximum nodes to concatenate

- pattern_rules:

  Named list of pattern replacement rules

## Value

Character string with the generated level name
