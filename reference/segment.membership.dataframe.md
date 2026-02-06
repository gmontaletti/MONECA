# Generate Segment Membership Dataframe from MONECA Results

Creates a dataframe showing segment membership for each row of the
original mobility matrix across all segmentation levels with positional
code format.

## Usage

``` r
segment.membership.dataframe(moneca_results, names = TRUE)
```

## Arguments

- moneca_results:

  A moneca object returned by
  [`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md)
  or `moneca_fast`.

- names:

  Logical indicating whether to generate descriptive labels for each
  level (default TRUE). When TRUE, creates additional columns with
  segment labels showing the representative node name and count of other
  members.

## Value

A dataframe with:

- name: The name of the row from the original matrix

- index: The index of the row in the original matrix

- level_X: For each segmentation level X (excluding level 1), the
  segment assignment using progressive numbering (001, 002, etc.)

- name_level_X: (when names=TRUE) Descriptive labels for each level
  showing the representative node and count of other members in format
  "NodeName (+N)"

- id_full: Positional code in format "xxx.xxx.xxx" representing
  hierarchical position

## Examples

``` r
# Generate example data
mx <- generate_mobility_data(n_classes = 5, immobility_strength = 0.7)

# Run moneca analysis
seg <- moneca_fast(mx, segment.levels = 3)
#>   |                                                                              |======================================================================| 100%
#>   |                                                                              |                                                                      |   0%

# Generate membership dataframe
membership_df <- segment.membership.dataframe(seg)
print(membership_df)
#>      name index level_2 name_level_2 level_3 name_level_3 level_4 name_level_4
#> 1 Class 1     1     002 Class 2 (+1)     002 Class 2 (+1)     001 Class 2 (+4)
#> 2 Class 2     2     002 Class 2 (+1)     002 Class 2 (+1)     001 Class 2 (+4)
#> 3 Class 3     3     003 Class 3 (+0)     001 Class 3 (+2)     001 Class 2 (+4)
#> 4 Class 4     4     001 Class 4 (+1)     001 Class 3 (+2)     001 Class 2 (+4)
#> 5 Class 5     5     001 Class 4 (+1)     001 Class 3 (+2)     001 Class 2 (+4)
#>           id_full
#> 1 001.002.002.001
#> 2 001.002.002.002
#> 3 001.001.003.003
#> 4 001.001.001.004
#> 5 001.001.001.005
```
