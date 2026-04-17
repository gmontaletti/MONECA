# Compute Pairwise Asymmetry from an Asymmetric RR Matrix

For every pair (i, j) where i \< j, computes \`abs(RR_ij - RR_ji) /
(RR_ij + RR_ji)\`.

## Usage

``` r
.pairwise_asymmetry(rr_asym)
```

## Arguments

- rr_asym:

  Asymmetric relative risk matrix (no NAs expected for within-segment
  pairs, but handled gracefully).

## Value

A data.frame with columns \`from\`, \`to\`, \`rr_ij\`, \`rr_ji\`,
\`asym\`.
