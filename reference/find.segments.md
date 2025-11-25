# Find Segments in Mobility Networks

Identifies discrete groups or segments based on a weighted network
matrix using a clique-based algorithm. This function implements the core
segmentation algorithm that iteratively assigns nodes to segments based
on network ties.

## Usage

``` r
find.segments(
  mat,
  cliques,
  cut.off = 1,
  mode = "symmetric",
  delete.upper.tri = TRUE
)
```

## Arguments

- mat:

  A weighted adjacency matrix representing mobility flows or
  relationships. Should include row and column names representing the
  categories/classes.

- cliques:

  A list of cliques (complete subgraphs) in the network, typically
  obtained from
  [`igraph::cliques()`](https://r.igraph.org/reference/cliques.html).

- cut.off:

  Numeric threshold for minimum weight or relative risk to be considered
  a network tie. Default is 1.

- mode:

  Character string specifying how to handle asymmetric relationships:

  - "symmetric" (default): Standard symmetric treatment

  - "Mutual": Only mutual ties (bidirectional) are considered

  - "Unmutual": Unidirectional ties are allowed

- delete.upper.tri:

  Logical indicating whether to process only the lower triangle of the
  matrix for efficiency. Default is TRUE.

## Value

A list with two components:

- membership:

  A factor indicating segment membership for each node

- cliques:

  A list where each element contains the indices of nodes belonging to
  that segment

## Details

The algorithm works by iteratively examining the strongest remaining
ties in the network and assigning nodes to segments based on clique
membership. It uses a greedy approach that prioritizes stronger
connections and larger existing segments.

## See also

[`moneca`](https://gmontaletti.github.io/MONECA/reference/moneca.md) for
the main analysis function
