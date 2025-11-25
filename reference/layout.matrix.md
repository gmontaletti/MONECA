# Layout matrix

A matrix with the coordinates of the segments using Fruchterman-Reingold
algorithm.

## Usage

``` r
layout.matrix(
  segments,
  attraction = c(320, 40, 10, 4, 2),
  level = seq(segments$segment.list),
  mode = "directed",
  weight.adjustment = 1,
  start.temp = 20,
  niter = 10000,
  tie.adjustment = 0.4,
  ...
)
```

## Arguments

- segments:

  A segment object from moneca().

- attraction:

  The distance between the segment points for each level.

- level:

  The included levels.

- mode:

  The mode (directed or undirected).

- weight.adjustment:

  Weight adjustment factor.

- start.temp:

  Starting temperature for the algorithm.

- niter:

  Number of iterations.

- tie.adjustment:

  Tie adjustment factor.

- ...:

  Additional arguments passed to layout algorithm.

## Value

A matrix of x,y coordinates for each node.
