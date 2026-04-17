# Compute Adjusted Rand Index Between Two Clusterings

Implements the ARI using the contingency table approach. Handles edge
cases where all items belong to a single cluster.

## Usage

``` r
.adjusted_rand_index(cl1, cl2)
```

## Arguments

- cl1:

  Integer or factor vector of cluster assignments (length n).

- cl2:

  Integer or factor vector of cluster assignments (length n).

## Value

Numeric scalar. ARI ranges from -1 to 1; 1 indicates perfect agreement,
0 indicates chance-level agreement.
