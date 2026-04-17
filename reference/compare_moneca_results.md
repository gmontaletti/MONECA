# Compare Two MONECA Clustering Results

Provides a comprehensive comparison of two moneca segmentation results
at a given hierarchical level. Metrics include segment sizes, pairwise
Jaccard overlap, Adjusted Rand Index, quality deltas, and asymmetry
deltas.

## Usage

``` r
compare_moneca_results(
  seg1,
  seg2,
  level = 2,
  labels = c("Result 1", "Result 2")
)
```

## Arguments

- seg1:

  A moneca object (first result).

- seg2:

  A moneca object (second result).

- level:

  Integer. Hierarchical level to compare (default 2).

- labels:

  Character vector of length 2. Human-readable labels for the two
  results (default \`c("Result 1", "Result 2")\`).

## Value

An S3 object of class \`"moneca_comparison"\` (a list) with:

- labels:

  Character vector of the two labels.

- level:

  Integer level compared.

- segment_sizes:

  A \`data.frame\` with columns \`label\`, \`segment_idx\`, and
  \`n_members\`.

- jaccard:

  A numeric matrix of Jaccard indices, rows correspond to seg1 segments
  and columns to seg2 segments.

- ari:

  Numeric scalar, the Adjusted Rand Index.

- quality_delta:

  A one-row \`data.frame\` with columns \`metric\`, \`label_1\`,
  \`mean_within_mobility_1\`, \`label_2\`, \`mean_within_mobility_2\`.
  Values are means of the \`within.mobility\` column of
  \[segment.quality()\] across segments. \`NULL\` if quality computation
  fails.

- asymmetry_delta:

  A list with components \`asymmetry_1\`, \`asymmetry_2\` (each the full
  \`data.frame\` returned by \[compute_asymmetry_scores()\]) and
  \`mean_asym_1\`, \`mean_asym_2\` (the mean asymmetry scores), or
  \`NULL\` if computation fails.

## Examples

``` r
if (FALSE) { # \dontrun{
mob <- generate_mobility_data(n_classes = 6, seed = 42)
seg1 <- moneca(mob, segment.levels = 3)
seg2 <- refine_segments(seg1, threshold = 0.3, level = 2)
cmp <- compare_moneca_results(seg1, seg2, level = 2,
  labels = c("Original", "Refined"))
print(cmp)
} # }
```
