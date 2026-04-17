# Evaluation: Directed Clustering Experiment

**Date**: 2026-03-16
**Branch**: `experimental/directed-clustering`
**Data**: 127x127 Italian profession mobility matrix (Lombardy, 7.6M transitions, 82% density)
**Preprocessing**: density reduction (SVD, k=16, 96.2% variance explained)

## 1. Computation Time

| Method | Median (s) | Range (s) | Overhead vs sum |
|--------|-----------|-----------|-----------------|
| `sum` (baseline) | 2.80 | 2.68 – 6.28 | — |
| `min` (min-reciprocity) | 1.48 | 1.35 – 1.50 | **-47%** (faster) |

The `min` method is substantially *faster* than the baseline. Min-reciprocity produces a sparser weight matrix (fewer edges above cutoff), which reduces clique enumeration time.

Asymmetry function timings (negligible overhead):

| Function | Time (s) |
|----------|----------|
| `compute_asymmetry_scores()` | 0.073 |
| `flag_asymmetric_segments()` | 0.073 |
| `refine_segments(threshold=0.3)` | 0.35 |
| `compare_moneca_results()` | 0.77 |

**Verdict**: No computation time concern. The `min` method is faster; asymmetry analysis functions add <1s total.

## 2. Code Complexity

### Lines of code added

| File | Lines | Purpose |
|------|-------|---------|
| `R/directed_analysis.R` | 802 | New file: 4 exported + 3 internal functions |
| `R/moneca_fast.R` | +62 net | `symmetric_method` parameter, branching in `make.segments.fast()` |
| `tests/testthat/test-directed-analysis.R` | 657 | Comprehensive test suite |
| `man/` (7 files) | ~280 | Auto-generated documentation |
| **Total** | ~1800 | |

### API surface increase

| Component | Count |
|-----------|-------|
| New exported functions | 4 (`compute_asymmetry_scores`, `flag_asymmetric_segments`, `refine_segments`, `compare_moneca_results`) |
| New S3 methods | 1 (`print.moneca_comparison`) |
| New internal functions | 3 (`.extract_submatrix_with_margins`, `.pairwise_asymmetry`, `.adjusted_rand_index`) |
| New parameters | 1 (`symmetric_method` in `moneca_fast()`) |

### Branching complexity in core algorithm

The `symmetric_method` parameter adds a single `if/else` branch in `make.segments.fast()` (lines 562-586 of moneca_fast.R). The `"sum"` path is identical to the original code. The `"min"` path calls `weight.matrix()` with `symmetric=FALSE`, then applies `pmin(mx, t(mx)) * 2`. No other function in the core algorithm is affected.

**Verdict**: Moderate code addition. The core algorithm change is minimal (one branch). The 800-line `directed_analysis.R` is self-contained and could be removed without touching any other file. Test coverage is thorough.

## 3. Profile Statistics

Based on `system.time()` across 3 runs each:

- **Sum method**: Time is dominated by clique enumeration in `find.segments.fast()` at level 1. The weight matrix computation (`weight.matrix()`) is <0.1s.
- **Min method**: Same profile but clique enumeration is faster because `pmin()` symmetrization produces fewer edges above `cut.off=1`. The extra `weight.matrix(..., symmetric=FALSE)` + `pmin()` call adds negligible time (<0.01s).
- **Memory**: Both methods allocate comparable memory. The `min` path computes one additional intermediate matrix (the asymmetric RR), which for a 127x127 matrix is ~130KB.

The `weight.matrix` double-call (asymmetric then `pmin`) is not a bottleneck.

## 4. Interpretability Gains

### 4.1 Clustering agreement (ARI)

| Level | ARI | Interpretation |
|-------|-----|----------------|
| 2 | 0.74 | Substantial agreement; ~70% of category pairs classified identically |
| 3 | 0.51 | Moderate agreement; methods diverge meaningfully at deeper levels |

At level 2, most segments are identical (top 5 Jaccard overlaps are 1.0). The differences concentrate in a few segments. At level 3, the `min` method produces 20 segments vs 17 for `sum`, with more evenly distributed sizes (max 11 vs max 15).

### 4.2 Segment structure

| Level | Sum | Min |
|-------|-----|-----|
| 2 | 34 segments (max size 5) | 33 segments (max size 4) |
| 3 | 17 segments (sizes: 15, 14, 10, 6, 5, 4, 4, 4, 3...) | 20 segments (sizes: 11, 9, 9, 7, 7, 6, 6, 6, 3...) |
| 4 | 12 segments (max size 24) | 12 segments (max size 20) |

The `min` method produces more, smaller segments at level 3, with a flatter size distribution.

### 4.3 Asymmetry detection

| Metric | Sum | Min |
|--------|-----|-----|
| Mean asymmetry (level 2) | 0.217 | 0.097 |
| Max asymmetry (level 2) | 1.000 | 0.451 |
| Segments with asym = 1.0 | 4 | 0 |
| Mean asymmetry (level 3) | 0.478 | 0.342 |

The `min` method eliminates all perfectly asymmetric segments (score = 1.0) at level 2. These are segments where at least one pair has relative risk in only one direction.

### 4.4 One-way bridges detected (sum method, level 2)

Four segments have perfect asymmetry (score = 1.0):

1. **Segment 9** (4 members): IT application specialists <-> IT technicians. The relative risk from specialists to technicians is nonzero but the reverse is zero — mobility is one-directional.

2. **Segment 10** (2 members): Professional training instructors/athletes <-> Recreational/cultural service workers. One-way flow.

3. **Segment 31** (3 members): Commercial distribution technicians <-> Tourism/hospitality technicians. One-way bridge.

4. **Segment 34** (3 members): Building painters/cleaners <-> Domestic service workers. One-way flow into domestic services.

These one-way bridges are substantively meaningful: they represent career transitions that happen in one direction but not the reverse (e.g., IT specialists move to IT technician roles but not vice versa, which reflects occupational classification effects in administrative data).

### 4.5 Refinement results

| Threshold | Segments flagged | Actual splits | Original -> Refined |
|-----------|-----------------|---------------|---------------------|
| 0.5 | 4 | 4 | 34 -> 35 |
| 0.3 | 6 | 6 | 34 -> 35 |

With threshold = 0.3, 6 segments are flagged but only 1 actual split occurs (segment 9, the IT cluster, splits into 2 sub-segments). The other 5 flagged segments are too small (2-3 members) to produce valid splits with `min_segment_size = 2`. This means `refine_segments()` has limited practical impact on this data: it identifies the problem but mostly cannot act on it because the segments are already small.

### 4.6 Quality metrics

Mean within-mobility scores are identical (64) for both methods. The segment quality metric does not differentiate between the two approaches, because it measures within-segment flow volume rather than directional consistency.

## 5. Recommendation

### Assessment

| Criterion | Sum (baseline) | Min (experiment) | Verdict |
|-----------|---------------|------------------|---------|
| Speed | 2.8s | 1.5s | Min is faster |
| Segment count | Comparable | Slightly more at L3 | Neutral |
| Asymmetry | High (4 segments at 1.0) | Low (max 0.45) | Min is cleaner |
| Quality (within-mobility) | 64 | 64 | Identical |
| Interpretability | One-way bridges present | One-way bridges removed | Depends on use case |
| Code complexity | — | +1 branch in core | Minimal |

### Decision: Keep `symmetric_method`, simplify asymmetry analysis

**Keep the `symmetric_method = "min"` parameter in `moneca_fast()`**. It is a clean, minimal addition (one branch) that:
- Produces faster execution
- Eliminates one-way bridge artifacts
- Generates more balanced segment sizes at deeper levels
- Does not affect backward compatibility (default remains `"sum"`)

**Simplify `directed_analysis.R`**. The post-hoc analysis functions have limited practical value:
- `refine_segments()` rarely produces actual splits because flagged segments are already small
- `compare_moneca_results()` is useful for evaluation but not for production use
- `compute_asymmetry_scores()` is the only function with clear diagnostic value

**Recommended path**:

1. **Merge `symmetric_method` parameter** to master as-is. Document it in the vignette as an option for matrices with strong directional asymmetry.

2. **Keep `compute_asymmetry_scores()` and `flag_asymmetric_segments()`** as diagnostic tools. They are lightweight, well-tested, and provide actionable information about segment quality.

3. **Demote `refine_segments()` and `compare_moneca_results()`** to internal functions or remove them. On the real data, `refine_segments()` achieves only 1 meaningful split out of 6 attempts. `compare_moneca_results()` is an evaluation tool that belongs in scripts, not in the package API.

4. **Do not change the default** from `"sum"` to `"min"`. The `"sum"` method is the published algorithm; changing the default would break reproducibility for existing users.

### Lessons learned

- Min-reciprocity is not just a theoretical improvement — it measurably reduces artifacts on real data.
- The asymmetry detection framework is sound for diagnostics, but the automated refinement step (`refine_segments`) needs larger segments to be effective. On fine-grained classifications (127 categories), level-2 segments are already small (2-5 members), leaving little room for splitting.
- The ARI of 0.74 at level 2 means the choice of symmetrization method matters but does not radically change the result. Most segments are stable across methods.
