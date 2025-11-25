# MONECA Package Performance Analysis Report

## Executive Summary

This report provides a detailed technical analysis of performance-critical code in the moneca package, identifying optimization opportunities in the core segmentation algorithms. The analysis focuses on two main implementations:

1. **find.segments()** in R/analytical_functions.R (original implementation)
2. **find.segments.fast()** in R/moneca_fast.R (optimized implementation)

---

## 1. Core Algorithm Complexity Analysis

### 1.1 find.segments() - Original Implementation

**Location:** R/analytical_functions.R, lines 39-155

**Current Complexity:**
- **Overall:** O(E × C × N) where:
  - E = number of edges above cutoff
  - C = number of cliques
  - N = number of nodes
- **Worst case:** O(E × C²) for dense graphs

**Critical Performance Issues:**

#### Issue 1: Nested Loop in Clique Testing (Lines 69-74)
```r
for (cl in cliques) {
  if (all(potential.clique %in% cl)) {
    return(TRUE)
  }
}
```
- **Complexity:** O(C × K) per call, where K = clique size
- **Called:** Once per edge processed (up to E times)
- **Total impact:** O(E × C × K)
- **Problem:** Sequential clique iteration with repeated subset operations

#### Issue 2: Edge Processing Main Loop (Lines 94-136)
```r
for (i in 1:loop.length){
  # Find maximum value
  max.ind <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
  max.mat[max.ind[1], max.ind[2]] <- NA

  # Group assignment logic
  group.candidates <- group[max.ind]
  group.members <- which(group %in% group.candidates)
  potential.clique <- unique(sort(c(group.members, candidate)))
  test <- clique.test(potential.clique)
}
```
- **Complexity:** O(E × N²)
- **Issues:**
  - `which(max.mat==max(...))` scans entire matrix each iteration: O(N²)
  - `which(group %in% group.candidates)` scans group vector: O(N)
  - Sequential edge processing prevents vectorization
  - Matrix modified in-place requiring repeated scans

#### Issue 3: Final Group Reorganization (Lines 146-150)
```r
l <- levels(g)
ud.list <- list()
for (i in 1:length(l)) ud.list[[i]] <- which(g == l[i])
```
- **Complexity:** O(L × N) where L = number of levels
- **Problem:** Could be replaced with split() vectorized operation

### 1.2 find.segments.fast() - Optimized Implementation

**Location:** R/moneca_fast.R, lines 115-258

**Improvements Made:**
1. **Pre-computed clique membership matrix** (lines 193-197)
2. **Vectorized edge extraction and sorting** (lines 174-187)
3. **Fast clique testing using matrix operations** (line 225)

**Remaining Bottlenecks:**

#### Bottleneck 1: Clique Membership Matrix Construction (Lines 194-197)
```r
clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
for (i in seq_along(cliques)) {
  clique_membership[i, cliques[[i]]] <- TRUE
}
```
- **Complexity:** O(C × K) where C = cliques, K = avg clique size
- **Memory:** O(C × N) - can be prohibitive for dense graphs
- **Optimization potential:** HIGH

#### Bottleneck 2: Edge Processing Loop Still Sequential (Lines 206-233)
```r
for (i in seq_len(nrow(edges))) {
  # ... group assignment logic ...
  is_clique <- any(apply(clique_membership[, potential_clique, drop = FALSE], 1, all))
}
```
- **Complexity:** O(E × C) for clique test per edge
- **Problem:** Still processes edges one-by-one
- **Optimization potential:** MEDIUM

---

## 2. Data Structure Analysis

### 2.1 Current Data Structures

| Structure | Type | Size | Usage | Efficiency |
|-----------|------|------|-------|------------|
| `mat` | matrix | N × N | Edge weights | Good |
| `cliques` | list of vectors | C × K̄ | Clique storage | Poor |
| `clique_membership` | logical matrix | C × N | Fast lookup | Memory-intensive |
| `group` | integer vector | N | Node assignments | Good |
| `edges` | matrix | E × 2 | Edge list | Good |

### 2.2 Opportunities for data.table

**High Priority Conversions:**

1. **Edge processing** could use data.table for:
   - Fast grouping operations
   - Reference semantics for in-place updates
   - Efficient sorted merges

2. **Clique membership** could use sparse matrix (Matrix package) or data.table:
   - Reduce memory for sparse clique structures
   - Faster subset operations

---

## 3. Specific Optimization Recommendations

### 3.1 CRITICAL: Vectorize Clique Membership Construction

**Current Code (moneca_fast.R, lines 194-197):**
```r
clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
for (i in seq_along(cliques)) {
  clique_membership[i, cliques[[i]]] <- TRUE
}
```

**Optimization Strategy:**
Use sparse matrix representation or vectorized construction:
```r
# Option 1: Sparse matrix (recommended for large graphs)
if (requireNamespace("Matrix", quietly = TRUE)) {
  clique_indices <- lapply(seq_along(cliques), function(i) {
    cbind(i, cliques[[i]])
  })
  clique_indices <- do.call(rbind, clique_indices)
  clique_membership <- Matrix::sparseMatrix(
    i = clique_indices[, 1],
    j = clique_indices[, 2],
    x = TRUE,
    dims = c(length(cliques), nrow(mat))
  )
} else {
  # Option 2: Vectorized matrix construction
  clique_membership <- matrix(FALSE, nrow = length(cliques), ncol = nrow(mat))
  indices <- unlist(lapply(seq_along(cliques), function(i) {
    cbind(i, cliques[[i]])
  }), recursive = FALSE)
  clique_membership[indices] <- TRUE
}
```

**Expected Improvement:** 2-5x faster for large clique sets, 50-80% memory reduction

### 3.2 HIGH PRIORITY: Convert Edge Processing to data.table

**Current Code (moneca_fast.R, lines 206-233):**
```r
for (i in seq_len(nrow(edges))) {
  edge <- edges[i, ]
  current_groups <- group[edge]
  # ... assignment logic ...
}
```

**Optimization Strategy:**
Use data.table for batch operations:
```r
library(data.table)

# Convert edges to data.table
edge_dt <- data.table(
  from = edges[, 1],
  to = edges[, 2],
  weight = edge_weights[edge_order],
  edge_id = seq_len(nrow(edges))
)

# Add group information
edge_dt[, from_group := group[from]]
edge_dt[, to_group := group[to]]

# Process in batches by group status
edge_dt[, both_unassigned := from_group == 0 & to_group == 0]
edge_dt[, at_least_one_assigned := from_group != 0 | to_group != 0]

# Vectorized processing for simple cases
# (detailed implementation would be more complex)
```

**Expected Improvement:** 3-10x faster for large edge sets

### 3.3 MEDIUM PRIORITY: Optimize Maximum Finding in Original find.segments()

**Current Code (analytical_functions.R, lines 110-111):**
```r
max.ind <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
max.mat[max.ind[1], max.ind[2]] <- NA
```

**Optimization Strategy:**
Pre-sort edges once instead of repeated maximum finding:
```r
# Before loop: extract and sort all edges once
valid_edges <- which(!is.na(mat) & mat > cut.off, arr.ind = TRUE)
edge_weights <- mat[valid_edges]
edge_order <- order(edge_weights, decreasing = TRUE)
sorted_edges <- valid_edges[edge_order, , drop = FALSE]

# Then iterate over sorted edges
for (i in seq_len(nrow(sorted_edges))) {
  edge <- sorted_edges[i, ]
  # ... process edge ...
}
```

**Expected Improvement:** 10-50x faster for large matrices (eliminates O(N²) per iteration)

### 3.4 LOW PRIORITY: Vectorize Group List Creation

**Current Code (analytical_functions.R, lines 146-150):**
```r
l <- levels(g)
ud.list <- list()
for (i in 1:length(l)) ud.list[[i]] <- which(g == l[i])
```

**Optimization Strategy:**
```r
ud.list <- unname(split(seq_along(g), g))
```

**Expected Improvement:** 2-3x faster, cleaner code

---

## 4. Memory Allocation Patterns

### 4.1 Current Memory Issues

1. **Pre-allocation in find.segments():**
   - `group` vector: GOOD (pre-allocated, line 60)
   - `clique_membership` matrix: POOR (large allocation for dense graphs)

2. **Repeated matrix copying in weight.matrix():**
   - Lines 571-588 (analytical_functions.R) already optimized
   - Uses in-place operations where possible

3. **List growth in segment creation:**
   - Lines 333-366 (analytical_functions.R) builds lists incrementally
   - Could benefit from pre-allocation if max size known

### 4.2 Memory Optimization Recommendations

**Recommendation 1: Implement Memory-Aware Clique Processing**
```r
# Detect if clique membership matrix would be too large
clique_memory_mb <- (length(cliques) * nrow(mat)) / 8 / 1024 / 1024
if (clique_memory_mb > 100) {
  # Use alternative algorithm (iterate cliques instead of pre-compute)
  warning("Large clique set detected, using memory-efficient mode")
  use_sparse <- TRUE
}
```

**Recommendation 2: Stream Processing for Very Large Graphs**
For graphs with >10,000 nodes, implement chunked processing:
- Process edges in batches
- Use disk-backed storage for intermediate results
- Consider ff or bigmemory packages

---

## 5. Algorithm Complexity Comparison

| Algorithm Component | Original | Optimized (Fast) | Optimal (Proposed) |
|---------------------|----------|------------------|-------------------|
| Edge extraction | O(E × N²) | O(N²) | O(N²) |
| Edge sorting | - | O(E log E) | O(E log E) |
| Clique membership | O(E × C × K) | O(C × K) + O(E × C) | O(C × K) sparse |
| Group assignment | O(E × N) | O(E × N) | O(E × log N) with data.table |
| **Total** | **O(E × C × N)** | **O(C × K + E × C)** | **O(C × K + E × log N)** |

### 5.1 Practical Performance Estimates

For typical social mobility matrices:
- N = 50-200 nodes (occupational categories)
- E = 500-2000 edges (significant ties)
- C = 100-1000 cliques
- K̄ = 3-5 average clique size

**Current Performance:**
- Original: 10-60 seconds for N=100
- Fast: 2-15 seconds for N=100

**Projected Performance (with proposed optimizations):**
- Optimized: 0.5-3 seconds for N=100
- Improvement: 5-20x faster than original, 2-5x faster than current fast version

---

## 6. Implementation Priorities

### Phase 1: Critical Optimizations (Highest Impact)
1. **Pre-sort edges** in find.segments() (Section 3.3)
   - Impact: 10-50x faster
   - Effort: Low (2-3 hours)
   - Risk: Low (maintains algorithmic correctness)

2. **Sparse clique membership** in find.segments.fast() (Section 3.1)
   - Impact: 50-80% memory reduction, 2-5x faster
   - Effort: Medium (4-6 hours)
   - Risk: Medium (requires Matrix package dependency)

### Phase 2: Major Refactoring (High Impact)
3. **data.table conversion** for edge processing (Section 3.2)
   - Impact: 3-10x faster
   - Effort: High (1-2 days)
   - Risk: High (requires significant testing)

4. **Batch clique testing** using matrix operations
   - Impact: 2-4x faster for dense graphs
   - Effort: High (1-2 days)
   - Risk: Medium (algorithmic changes)

### Phase 3: Polish (Lower Impact)
5. **Vectorize group list creation** (Section 3.4)
   - Impact: 2-3x faster (small overall impact)
   - Effort: Low (15 minutes)
   - Risk: None

6. **Memory-aware mode switching**
   - Impact: Prevents crashes on large graphs
   - Effort: Medium (4-6 hours)
   - Risk: Low

---

## 7. Specific Code Locations for Optimization

### High Priority Lines:

**R/analytical_functions.R:**
- Lines 69-74: Clique testing loop (CRITICAL)
- Lines 94-136: Edge processing loop (CRITICAL)
- Lines 110-111: Maximum finding (HIGH)
- Lines 146-150: Group list creation (LOW)

**R/moneca_fast.R:**
- Lines 194-197: Clique membership construction (CRITICAL)
- Lines 206-233: Edge processing loop (HIGH)
- Line 225: Clique testing with apply (MEDIUM)

**R/analytical_functions.R (weight.matrix):**
- Lines 570-588: Already well-optimized (MAINTAIN)

---

## 8. Testing Requirements

For each optimization:
1. **Correctness testing:** Ensure identical results to original
2. **Performance benchmarking:** Use microbenchmark package
3. **Memory profiling:** Use profmem or bench packages
4. **Edge case testing:**
   - Empty graphs
   - Complete graphs
   - Single-node graphs
   - Very sparse graphs
   - Very dense graphs

---

## 9. Recommended Next Steps

1. **Immediate (Week 1):**
   - Implement pre-sorted edge processing (Section 3.3)
   - Add benchmarking suite
   - Profile current code with realistic datasets

2. **Short-term (Weeks 2-3):**
   - Implement sparse clique membership (Section 3.1)
   - Add memory monitoring
   - Create performance comparison vignette

3. **Medium-term (Month 2):**
   - Evaluate data.table conversion (Section 3.2)
   - Implement if benefits justify complexity
   - Create adaptive algorithm selection

4. **Long-term (Months 3+):**
   - Investigate parallel processing options
   - Consider Rcpp for critical loops
   - Implement streaming algorithms for very large graphs

---

## 10. Benchmarking Framework

Recommended benchmark code structure:

```r
library(microbenchmark)
library(moneca)

# Generate test datasets of varying sizes
test_sizes <- c(10, 20, 50, 100, 200)
test_data <- lapply(test_sizes, function(n) {
  generate_mobility_data(n_classes = n, seed = 123)
})

# Benchmark each implementation
results <- lapply(test_data, function(mx) {
  microbenchmark(
    original = moneca(mx, segment.levels = 3),
    fast = moneca_fast(mx, segment.levels = 3),
    times = 10
  )
})

# Profile memory usage
library(profmem)
mem_profile <- profmem({
  moneca_fast(test_data[[3]], segment.levels = 3)
})
```

---

## 11. Conclusion

The moneca package has significant optimization opportunities, particularly in the core segmentation algorithm. The most impactful improvements would come from:

1. **Pre-sorting edges** (eliminates repeated O(N²) matrix scans)
2. **Sparse clique representation** (reduces memory and computation)
3. **data.table conversion** (vectorizes group operations)

These optimizations could deliver 5-20x performance improvement while maintaining algorithmic correctness. The phased implementation approach allows for incremental improvements with controlled risk.

The current moneca_fast implementation already shows good optimization practices (vectorized edge extraction, pre-computed membership), but still has room for substantial improvement in the edge processing loop and memory usage.

---

**Report Generated:** 2025-11-25
**Analysis Version:** 1.0
**Package Version Analyzed:** moneca 0.9.2
