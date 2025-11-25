# MONECA Package Revision Plan

**Author**: Giampaolo Montaletti
**Date**: 2025-11-25
**Version**: 0.9.2 → 1.0.0

---

## Executive Summary

This plan outlines a comprehensive revision of the moneca package to:
1. Streamline the API to a coherent core focused on clustering and segment membership
2. Optimize performance through vectorization and data.table
3. Restructure unit tests for maintainability
4. Consolidate documentation

---

## Phase 1: API Consolidation (Export Reduction)

### Current State
- **72 exported functions** + 17 S3 methods
- Functions span: clustering, tuning, plotting, diagnostics, temporal analysis, utilities

### Target State
Reduce to **~20 core exports** organized into coherent functional groups.

### Proposed Core API

#### Tier 1: Essential Clustering (8 functions)
| Function | Purpose | Status |
|----------|---------|--------|
| `moneca()` | Original clustering algorithm | KEEP (unchanged per CLAUDE.md) |
| `moneca_fast()` | Optimized single-core implementation | KEEP (optimize further) |
| `weight.matrix()` | Create relative risk matrix | KEEP |
| `segment.membership()` | Extract segment assignments | KEEP |
| `segment.membership.dataframe()` | Segments as tidy dataframe | KEEP |
| `segment.quality()` | Quality metrics | KEEP |
| `segment.edges()` | Extract weighted edges | KEEP |
| `generate_mobility_data()` | Synthetic data generation | KEEP |

#### Tier 2: Visualization (5 functions)
| Function | Purpose | Status |
|----------|---------|--------|
| `plot_moneca_ggraph()` | Modern network visualization | KEEP |
| `plot_ego_ggraph()` | Ego network visualization | KEEP |
| `plot_stair_ggraph()` | Multi-level visualization | KEEP |
| `plot_moneca_dendrogram()` | Hierarchical dendrogram | KEEP |
| `plot_segment_quality()` | Quality visualization | KEEP |

#### Tier 3: Tuning (4 functions)
| Function | Purpose | Status |
|----------|---------|--------|
| `auto_tune_small_cell_reduction()` | Main parameter tuning | KEEP |
| `find_optimal_cutoff()` | Optimal cutoff determination | KEEP |
| `analyze_rr_distribution()` | Relative risk analysis | KEEP |
| `plot_tuning_results()` | Tuning visualization | KEEP |

#### Tier 4: Utilities (3 functions)
| Function | Purpose | Status |
|----------|---------|--------|
| `segment.colors()` | Color generation for segments | KEEP |
| `layout.matrix()` | Layout computation | KEEP |
| `vertex.mobility()` | Node mobility metrics | KEEP |

### Functions to Move to Internal (Not Exported)

#### Move to Internal (~35 functions)
```r
# Low-level algorithms (internal implementation details)
find.segments()              # Used internally by moneca/moneca_fast
find.segments.parallel()     # Internal parallel implementation
force.segments()             # Internal forcing mechanism
segment.matrix.parallel()    # Internal aggregation
weight.matrix.parallel()     # Internal parallel computation

# Diagnostic utilities (developer-focused)
test_igraph_compatibility()
analyze_package_dependencies()
validate_description_dependencies()
generate_dependency_report()
diagnose_parallel_execution()
monitor_system_resources()
benchmark_parallel_performance()
detect_system_resources()
get_optimal_cores()
should_use_parallel()
clear_evaluation_caches()
get_cache_stats()

# Advanced tuning (niche use cases)
auto_tune_small_cell_reduction_enhanced()
auto_tune_joint_parameters()
assess_clustering_stability()
assess_clustering_stability_parallel()
compute_tuning_metrics()
compute_clustering_quality_metrics()
generate_candidate_values()
analyze_parameter_interaction()
plot_optimization_surface()
suggest_parameter_ranges()
evaluate_performance_trade_offs()
cutoff_sensitivity_analysis()
evaluate_cutoff_strength()
calculate_strength_density()
parameter_sensitivity_plot()
performance_quality_tradeoff_plot()
plot_cutoff_analysis()
plot_cutoff_interactive()
plot_rr_distribution()
plot_strength_comparison()
plot_elbow_curve()
```

### Functions to Deprecate/Remove (~15 functions)

```r
# Parallel variants (consolidate into main functions with parallel argument)
moneca_parallel()         # Merge into moneca_fast(parallel = TRUE)

# Temporal analysis (spin off to separate package or vignette-only)
moneca_temporal()
temporal_stability_analysis()
export_node_trajectories()
plot_temporal_segments()
plot_temporal_stability()
generate_temporal_report()

# Maximal cliques variant (experimental)
new_moneca()              # Document as experimental internal

# Legacy plotting (superseded by ggraph versions)
gg.moneca()               # Soft deprecate, keep working
ego.plot()                # Soft deprecate
stair.plot()              # Soft deprecate
moneca.plot()             # Soft deprecate

# Redundant membership functions
segment.membership.enhanced()  # Merge into segment.membership()
first.level.summary()          # Integrate into print.moneca()

# Example dataset generator (too specific)
generate_example_datasets()    # Keep generate_mobility_data() only
```

### Implementation Steps

1. **Create internal function wrappers**
   - Move functions to internal namespace
   - Add `@keywords internal` roxygen tags
   - Update calling code

2. **Soft deprecation for legacy functions**
   - Add `.Deprecated()` calls with migration guidance
   - Keep working for 2 versions

3. **Merge redundant functions**
   - `segment.membership.enhanced()` → options in `segment.membership()`
   - `moneca_parallel()` → `moneca_fast(parallel = TRUE)`

---

## Phase 2: Performance Optimization

### Critical Bottlenecks Identified

#### Bottleneck 1: `find.segments()` Main Loop
**Location**: `R/analytical_functions.R:94-136`
**Complexity**: O(n²) where n = edges above cutoff
**Current Implementation**:
```r
for (i in 1:loop.length){
    max.ind <- which(max.mat==max(max.mat, na.rm=TRUE), arr.ind=TRUE)[1,]
    # ... sequential processing
}
```

**Optimization Strategy**:
1. Pre-compute all edge values and sort once
2. Use vectorized clique membership testing
3. Leverage data.table for fast lookups

#### Bottleneck 2: `clique.test()` Inner Function
**Location**: `R/analytical_functions.R:65-75`
**Complexity**: O(k) per call where k = number of cliques
**Current Implementation**:
```r
for (cl in cliques) {
    if (all(potential.clique %in% cl)) {
        return(TRUE)
    }
}
```

**Optimization Strategy**:
1. Pre-build clique membership matrix (already in moneca_fast)
2. Use matrix operations for subset testing
3. Implement early termination with sorted cliques

#### Bottleneck 3: Edge Processing Loop
**Location**: `R/moneca_fast.R:211-237`
**Issue**: Sequential dependency in segment assignment

**Optimization Strategy**:
1. Batch process independent edges
2. Use data.table for fast grouping operations
3. Implement incremental segment updates

### Proposed Optimized Implementation

#### New Function: `find_segments_optimized()`

```r
#' @title Optimized Segment Finding
#' @description Vectorized segment identification using data.table
#' @keywords internal
find_segments_optimized <- function(mat, cutoff.rr, cliques, verbose = FALSE) {

  # 1. Pre-compute: Convert to data.table of edges
  edges_dt <- data.table::as.data.table(
    which(mat > cutoff.rr & !is.na(mat), arr.ind = TRUE)
  )
  edges_dt[, weight := mat[cbind(row, col)]]
  edges_dt[, edge_id := .I]
  data.table::setkey(edges_dt, weight)

  # 2. Pre-build clique membership lookup
  n_nodes <- nrow(mat)
  clique_matrix <- matrix(FALSE, nrow = length(cliques), ncol = n_nodes)
  for (i in seq_along(cliques)) {
    clique_matrix[i, cliques[[i]]] <- TRUE
  }

  # 3. Vectorized clique membership test function
  test_in_clique <- function(node_pairs) {
    # Returns logical vector: TRUE if pair is in any clique
    apply(node_pairs, 1, function(pair) {
      any(clique_matrix[, pair[1]] & clique_matrix[, pair[2]])
    })
  }

  # 4. Sort edges by weight (descending) - single sort operation
  edges_dt <- edges_dt[order(-weight)]

  # 5. Process edges in batches where possible
  segments <- integer(n_nodes)
  current_segment <- 0L

  # Group processing with data.table
  edges_dt[, in_clique := test_in_clique(cbind(row, col))]

  # Sequential assignment (necessary for correctness)
  for (i in seq_len(nrow(edges_dt))) {
    if (!edges_dt$in_clique[i]) next

    row_i <- edges_dt$row[i]
    col_i <- edges_dt$col[i]

    seg_row <- segments[row_i]
    seg_col <- segments[col_i]

    if (seg_row == 0L && seg_col == 0L) {
      current_segment <- current_segment + 1L
      segments[row_i] <- current_segment
      segments[col_i] <- current_segment
    } else if (seg_row == 0L) {
      segments[row_i] <- seg_col
    } else if (seg_col == 0L) {
      segments[col_i] <- seg_row
    }
    # Both already assigned - skip (segment merge handled later)
  }

  # 6. Assign singletons
  singletons <- which(segments == 0L)
  for (s in singletons) {
    current_segment <- current_segment + 1L
    segments[s] <- current_segment
  }

  return(segments)
}
```

#### Linear Algebra Optimization: Weight Matrix

```r
#' @title Optimized Weight Matrix Computation
#' @description Vectorized relative risk calculation
#' @keywords internal
weight_matrix_optimized <- function(mat, cutoff.rr = 1, small.cell.reduction = 1) {

  # Vectorized row/column totals
  row_totals <- rowSums(mat, na.rm = TRUE)
  col_totals <- colSums(mat, na.rm = TRUE)
  grand_total <- sum(mat, na.rm = TRUE)

  # Expected values (outer product for vectorization)
  expected <- outer(row_totals, col_totals) / grand_total

  # Small cell adjustment (vectorized)
  mat_adj <- mat
  mat_adj[mat < small.cell.reduction] <- small.cell.reduction
  expected[expected < small.cell.reduction] <- small.cell.reduction

  # Relative risk (element-wise)
  rr <- mat_adj / expected

  # Apply cutoff (vectorized)
  rr[rr < cutoff.rr] <- NA

  return(rr)
}
```

### Performance Testing Requirements

1. **Benchmark Suite**
   - Create standardized test matrices (100x100, 500x500, 1000x1000)
   - Measure time and memory for each implementation
   - Verify result equivalence with `all.equal()`

2. **Regression Tests**
   - All existing test cases must pass with identical results
   - Add explicit numerical tolerance tests

3. **Expected Improvements**
   - Target: 5-10x speedup for large matrices (>500 categories)
   - Memory: Reduce peak memory by avoiding intermediate copies

---

## Phase 3: Unit Test Restructuring

### Current State
- **20 test files** with significant overlap
- Total ~170,000 lines of test code
- Some files >10,000 lines (test-auto-tuning.R: 20,900 lines)

### Problems Identified
1. Test files too large and monolithic
2. Duplicate test logic across files
3. No clear test hierarchy (unit vs integration)
4. Performance tests mixed with correctness tests

### Proposed Test Structure

```
tests/
├── testthat/
│   ├── unit/
│   │   ├── test-weight-matrix.R          # weight.matrix() unit tests
│   │   ├── test-find-segments.R          # find.segments() unit tests
│   │   ├── test-segment-membership.R     # membership extraction tests
│   │   ├── test-segment-quality.R        # quality metrics tests
│   │   ├── test-synthetic-data.R         # data generation tests
│   │   └── test-igraph-compat.R          # compatibility layer tests
│   │
│   ├── integration/
│   │   ├── test-moneca-pipeline.R        # Full clustering pipeline
│   │   ├── test-moneca-fast-equiv.R      # moneca vs moneca_fast equivalence
│   │   ├── test-visualization.R          # Plotting integration tests
│   │   └── test-tuning-workflow.R        # Auto-tuning workflow
│   │
│   ├── performance/
│   │   ├── test-benchmark-clustering.R   # Clustering benchmarks
│   │   ├── test-benchmark-optimization.R # Optimization validation
│   │   └── test-memory-usage.R           # Memory profiling tests
│   │
│   ├── regression/
│   │   ├── test-known-results.R          # Fixed input/output tests
│   │   ├── test-edge-cases.R             # Boundary conditions
│   │   └── test-infinite-loop.R          # Regression for fixed bugs
│   │
│   └── helpers/
│       ├── helper-test-data.R            # Shared test data generators
│       ├── helper-assertions.R           # Custom test assertions
│       └── helper-fixtures.R             # Test fixtures
│
└── testthat.R
```

### Test Consolidation Plan

| Old Files | New Location | Notes |
|-----------|--------------|-------|
| test-moneca-core.R | unit/test-weight-matrix.R, unit/test-find-segments.R | Split by function |
| test-moneca-fast.R | integration/test-moneca-fast-equiv.R | Focus on equivalence |
| test-algorithm-equivalence.R | integration/test-moneca-fast-equiv.R | Merge |
| test-plotting.R | integration/test-visualization.R | Keep focused |
| test-synthetic-data.R | unit/test-synthetic-data.R | Keep |
| test-igraph-compatibility.R | unit/test-igraph-compat.R | Keep |
| test-auto-tuning*.R (4 files) | integration/test-tuning-workflow.R | Consolidate |
| test-cutoff-strength.R | unit/test-segment-quality.R | Integrate |
| test-parallel-*.R (3 files) | Remove or minimal | Parallel is internal |
| test-temporal-*.R (3 files) | Remove | Temporal is deprecated |
| test-infinite-loop-regression.R | regression/test-infinite-loop.R | Keep |
| test-new-moneca.R | Remove or internal | new_moneca is internal |

### Test Helper Functions

```r
# helper-test-data.R

#' Generate standard test matrices
#' @keywords internal
generate_test_matrix <- function(size = c("small", "medium", "large"),
                                  type = c("simple", "complex", "sparse")) {
  size <- match.arg(size)
  type <- match.arg(type)

  n <- switch(size,
    small = 5,
    medium = 10,
    large = 20
  )

  seed <- switch(type,
    simple = 123,
    complex = 456,
    sparse = 789
  )

  generate_mobility_data(n_classes = n, seed = seed)
}

#' Assert moneca results equivalence
#' @keywords internal
expect_moneca_equal <- function(result1, result2, tolerance = 1e-10) {
  expect_equal(
    result1$segment.list,
    result2$segment.list,
    tolerance = tolerance
  )
  expect_equal(
    length(result1$mat.list),
    length(result2$mat.list)
  )
}
```

### Coverage Targets

| Category | Target | Measurement |
|----------|--------|-------------|
| Core functions | 95% | Line coverage |
| Visualization | 80% | Branch coverage |
| Tuning | 85% | Line coverage |
| Overall | 90% | Line coverage |

---

## Phase 4: Documentation Rework

### Current State
- 5 vignettes (some overlap)
- README.md with installation and quick start
- Roxygen2 documentation for all exported functions

### Problems
1. Too many vignettes for a focused package
2. Documentation spread across files
3. Some examples use deprecated patterns

### Proposed Documentation Structure

#### Vignettes (Consolidate to 2)

1. **moneca-introduction.Rmd** (KEEP, REVISE)
   - Package overview
   - Installation
   - Basic workflow
   - Interpreting results
   - Visualization options

2. **moneca-advanced.Rmd** (NEW, from merging)
   - Parameter tuning
   - Performance optimization
   - Working with large matrices
   - Custom visualization

#### Remove/Archive
- algorithm-comparison.Rmd → Archive to reference/
- moneca-statistics.Rmd → Integrate into introduction
- auto-tuning-guide.Rmd → Merge into advanced
- parameter-calibration.Rmd → Merge into advanced

### Function Documentation Updates

#### Priority 1: Core Functions (Full Rewrite)
- `moneca()` - Complete examples, parameter explanations
- `moneca_fast()` - Performance comparison, when to use
- `weight.matrix()` - Mathematical explanation
- `segment.membership()` - All output formats
- `plot_moneca_ggraph()` - Comprehensive visualization guide

#### Priority 2: Update Examples
- Replace legacy data with `generate_mobility_data()`
- Show modern ggraph plotting in all examples
- Add practical use cases

#### Priority 3: Cross-references
- Add `@seealso` to all related functions
- Link to vignettes from function docs
- Add `@family` tags for grouping

### README.md Updates

```markdown
# moneca

Mobility Network Clustering Analysis - Create weighted networks from
mobility tables and use cliques to create discrete and nested clusters.

## Installation

```r
# Install from GitHub
remotes::install_github("gmontaletti/moneca")
```

## Quick Start

```r
library(moneca)

# Generate example data
data <- generate_mobility_data(n_classes = 10, seed = 42)

# Run clustering
result <- moneca_fast(data, segment.levels = 3)

# Get segment memberships
segments <- segment.membership(result)

# Visualize
plot_moneca_ggraph(result)
```

## Core Functions

| Function | Purpose |
|----------|---------|
| `moneca_fast()` | Fast hierarchical clustering |
| `segment.membership()` | Extract cluster assignments |
| `plot_moneca_ggraph()` | Network visualization |

## Citation

```bibtex
@software{moneca2025,
  author = {Montaletti, Giampaolo},
  title = {moneca: Mobility Network Clustering Analysis},
  version = {1.0.0},
  year = {2025},
  url = {https://github.com/gmontaletti/moneca}
}
```
```

---

## Implementation Schedule

### Phase 1: API Consolidation
- [ ] Audit all exported functions
- [ ] Move internal functions
- [ ] Add deprecation warnings
- [ ] Update NAMESPACE
- [ ] Test backward compatibility

### Phase 2: Performance Optimization
- [ ] Implement `find_segments_optimized()`
- [ ] Implement `weight_matrix_optimized()`
- [ ] Add data.table dependency
- [ ] Create benchmark suite
- [ ] Validate result equivalence
- [ ] Profile memory usage

### Phase 3: Test Restructuring
- [ ] Create new test directory structure
- [ ] Write helper functions
- [ ] Migrate and consolidate tests
- [ ] Remove deprecated test files
- [ ] Verify coverage targets

### Phase 4: Documentation
- [ ] Consolidate vignettes
- [ ] Update function documentation
- [ ] Revise README.md
- [ ] Update CLAUDE.md
- [ ] Build and check documentation

### Final Steps
- [ ] Version bump to 1.0.0
- [ ] Update DESCRIPTION
- [ ] Full R CMD check
- [ ] Update citation in README
- [ ] Create release notes

---

## Risk Mitigation

1. **Result Compatibility**
   - All optimization changes must produce bit-identical results to original
   - Comprehensive regression test suite before any changes
   - `all.equal()` checks in CI

2. **Breaking Changes**
   - Soft deprecation with 2-version grace period
   - Clear migration documentation
   - Keep working aliases for removed functions

3. **Performance Regression**
   - Benchmark on standardized test data
   - Automated performance tests in CI
   - Fallback to original implementation if issues

---

## Appendix: Files to Archive

Move to `../reference/moneca/` (per CLAUDE.md):

```
R/parallel_moneca.R
R/parallel_diagnostics.R
R/smart_parallel_switching.R
R/temporal_clustering.R
R/temporal_stability.R
R/temporal_plotting.R
R/auto_tuning_enhanced.R
R/auto_tuning_helpers.R
R/auto_tuning_visualization.R
R/joint_tuning.R
R/multi_objective_optimization.R
R/cutoff_rr_analysis.R
R/cutoff_rr_visualization.R
R/cutoff_sensitivity_strength.R
R/advanced_optimization.R
R/test_dependency_updates.R
vignettes/algorithm-comparison.Rmd
vignettes/moneca-statistics.Rmd
vignettes/auto-tuning-guide.Rmd
vignettes/parameter-calibration.Rmd
```

Test files to archive:
```
tests/testthat/test-parallel-*.R
tests/testthat/test-temporal-*.R
tests/testthat/test-auto-tuning*.R (merge first)
tests/testthat/test-joint-tuning.R
tests/testthat/test-enhanced-joint-tuning.R
tests/testthat/test-bayesian-parallel-behavior.R
tests/testthat/test-new-moneca.R
```
