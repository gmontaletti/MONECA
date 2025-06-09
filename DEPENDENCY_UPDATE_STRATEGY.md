# MONECA Package Dependency Update Strategy

## Overview
This document outlines the comprehensive strategy for updating package dependencies in MONECA, with a focus on the igraph package migration from deprecated to modern API functions.

## Deep Analysis: Pros and Cons

### 1. Dependency Management Approach

#### Current Situation
- Package uses `Depends:` for all packages instead of `Imports:`
- Missing explicit version requirements
- Some packages (RColorBrewer, scales) used but not declared
- Using deprecated igraph functions (dot notation vs underscore notation)

#### Chosen Solution: Hybrid Compatibility Layer

**Pros:**
- **Backward Compatibility**: Works with igraph >= 1.3.0, supporting users with older R installations
- **Forward Compatibility**: Ready for future igraph versions that may remove deprecated functions
- **Zero Breaking Changes**: Existing code continues to work without modification
- **Gradual Migration**: Can update functions incrementally without risk
- **Testing Safety**: Can validate each function mapping before deployment
- **Documentation**: Compatibility layer serves as documentation of API changes

**Cons:**
- **Maintenance Overhead**: Need to maintain compatibility layer
- **Performance**: Minimal overhead from function wrapping (negligible in practice)
- **Complexity**: Additional abstraction layer to understand
- **Testing Burden**: Need to test against multiple igraph versions

### 2. Alternative Approaches Considered

#### A. Direct Replacement (Not Chosen)
**Pros:**
- Cleaner code
- Direct use of modern API
- No abstraction layer

**Cons:**
- **Breaking changes** for users with older igraph
- Risk of subtle behavior differences
- All-or-nothing approach
- Difficult to rollback

#### B. Conditional Code Blocks (Not Chosen)
```r
if (packageVersion("igraph") >= "1.3.0") {
  g <- graph_from_adjacency_matrix(...)
} else {
  g <- graph.adjacency(...)
}
```

**Pros:**
- No abstraction needed
- Clear version handling

**Cons:**
- Code duplication throughout package
- Difficult to maintain
- Cluttered codebase

#### C. Force Modern igraph (Not Chosen)
Require igraph >= 2.0.0 in DESCRIPTION

**Pros:**
- Use latest features
- Simplified code
- Better performance

**Cons:**
- **Excludes many users** on older R/igraph versions
- Not necessary for MONECA's functionality
- Poor user experience

### 3. Implementation Details

#### Compatibility Layer Design
```r
# Smart function mapping based on igraph version
.onLoad <- function(libname, pkgname) {
  if (igraph_version >= "1.3.0") {
    # Map to new functions
  } else {
    # Use old functions
  }
}
```

**Benefits:**
1. **Single point of truth** for version handling
2. **Testable** - can mock different versions
3. **Extensible** - easy to add new mappings
4. **Transparent** - users don't see the complexity

#### Custom norm_coords Implementation
The `norm_coords` function was removed from igraph, so we provide our own:

```r
norm_coords.compat <- function(layout, xmin = -1, xmax = 1, ymin = -1, ymax = 1) {
  # Efficient normalization algorithm
  # Handles edge cases (single point, identical coordinates)
}
```

**Design Decisions:**
- Matches original igraph behavior exactly
- Optimized for common use cases
- Includes parameter validation
- Well-documented algorithm

### 4. Migration Strategy

#### Phase 1: Infrastructure (Completed)
✓ Create compatibility layer
✓ Update DESCRIPTION with proper imports
✓ Add comprehensive package documentation
✓ Create migration and testing tools

#### Phase 2: Validation (Next Steps)
- Run test suite across igraph versions
- Analyze performance impact
- Document behavior differences

#### Phase 3: Implementation
- Apply migrations using automated tools
- Update documentation
- Release candidate testing

#### Phase 4: User Communication
- Migration guide for users
- Deprecation warnings for direct old function use
- Version compatibility matrix

### 5. Dependency Declaration Best Practices

#### From Depends to Imports
**Change:**
```
Depends: R (>= 3.0.0), ggplot2, igraph, ...
```
**To:**
```
Depends: R (>= 3.5.0)
Imports: ggplot2 (>= 3.4.0), igraph (>= 1.3.0), ...
```

**Rationale:**
- `Depends` attaches packages to user's environment (pollutes namespace)
- `Imports` keeps packages internal (cleaner)
- R >= 3.5.0 provides better namespace handling
- Version requirements ensure compatibility

#### Missing Dependencies Added
- **RColorBrewer**: For color palettes in visualizations
- **scales**: For axis transformations in plots
- **stats, grDevices**: For core R functions (best practice to declare)

### 6. Testing Framework Benefits

The comprehensive testing framework provides:

1. **Compatibility Testing**: Verify functions work across versions
2. **Usage Analysis**: Understand which functions need updating
3. **Dependency Auditing**: Ensure all used packages are declared
4. **Migration Validation**: Confirm updates don't break functionality
5. **Reporting**: Clear documentation of changes and status

### 7. Long-term Maintenance Strategy

#### Version Support Policy
- Support igraph versions from past 3 years
- Test against latest and oldest supported versions
- Gradual deprecation with warnings

#### Monitoring
- Automated testing in CI/CD
- Version compatibility matrix in documentation
- User feedback channels

#### Future-proofing
- Abstract complex operations into package functions
- Minimize direct igraph calls where possible
- Regular dependency audits

## Conclusion

This comprehensive approach balances multiple concerns:
- **User experience**: No breaking changes
- **Maintainability**: Clear, documented compatibility layer
- **Future-proofing**: Ready for API changes
- **Performance**: Minimal overhead
- **Testing**: Comprehensive validation

The strategy goes beyond basic updates to create a robust, maintainable solution that serves users across different environments while preparing for future changes.