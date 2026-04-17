# Split an Asymmetric Segment by Removing High-Asymmetry Edges

Attempts to split a single segment into sub-groups by iteratively
removing the edges with the highest pairwise asymmetry until the
underlying graph disconnects into valid components (each with at least
\`min_segment_size\` members).

## Usage

``` r
split_asymmetric_segment(
  segments,
  segment_idx,
  level,
  max_splits = 2,
  min_segment_size = 2
)
```

## Arguments

- segments:

  A moneca object.

- segment_idx:

  Integer index of the segment within
  \`segments\$segment.list\[\[level\]\]\`.

- level:

  Integer hierarchical level.

- max_splits:

  Maximum number of resulting components (default 2).

- min_segment_size:

  Minimum number of members per resulting component (default 2).

## Value

A list of integer vectors, each containing the member indices of a
resulting sub-segment. If no valid split is found, returns a list
containing the original member vector unchanged.
