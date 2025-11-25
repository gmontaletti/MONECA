# File Archival Report

**Date**: November 25, 2025
**Operation**: Archive R files to reference directory
**Status**: COMPLETED SUCCESSFULLY

## Operation Summary

Successfully archived 17 R files from the moneca package to the reference directory, cleaning up the active development codebase.

**Archive Location**: `/Users/giampaolomontaletti/Documents/funzioni/reference/moneca/archived_R/`

## Files Successfully Archived

### Parallel Processing Module (3 files)
- parallel_moneca.R (25.3 KB)
- parallel_diagnostics.R (36.1 KB)
- smart_parallel_switching.R (28.7 KB)

### Temporal Analysis Module (3 files)
- temporal_clustering.R (32.3 KB)
- temporal_stability.R (16.6 KB)
- temporal_plotting.R (20.5 KB)

### Auto-tuning Module (4 files)
- auto_tuning_enhanced.R (18.1 KB)
- auto_tuning_helpers.R (11.5 KB)
- auto_tuning_visualization.R (19.8 KB)
- joint_tuning.R (139.5 KB)

### Advanced Optimization Module (5 files)
- multi_objective_optimization.R (11.7 KB)
- advanced_optimization.R (17.2 KB)
- cutoff_rr_analysis.R (10.0 KB)
- cutoff_rr_visualization.R (15.2 KB)
- cutoff_sensitivity_strength.R (21.8 KB)

### Testing & Data Generation (2 files)
- test_dependency_updates.R (7.9 KB)
- data_generation.R (1.3 KB)

**Total Files Archived**: 17
**Total Archive Size**: 472 KB

## Files Not Found (0)
All requested files were successfully located and archived. No files were missing.

## Current Active R Files (11)

The following files remain in `/Users/giampaolomontaletti/Documents/funzioni/MONECA/R/`:

1. **moneca.R** - Main package documentation and exports
2. **analytical_functions.R** - Core algorithm implementations
3. **descriptive_functions.R** - Descriptive statistics
4. **modern_plotting.R** - Modern ggraph visualization system
5. **ggplotting.R** - Legacy ggplot2 functions (backward compatibility)
6. **synthetic_data.R** - Data generation functions
7. **igraph_compatibility.R** - igraph version compatibility layer
8. **moneca_original.R** - Original reference implementation
9. **moneca_fast.R** - Optimized fast implementation
10. **auto_tuning.R** - Main auto-tuning (retained as core feature)
11. **new_moneca.R** - New functionality under development

## Git Status

Files have been removed from git tracking using `git rm --cached` to:
- Preserve their commit history
- Clean up the active repository
- Maintain ability to reference archived versions through git history

**Staged deletions**: 17 R files ready for commit
**Additional cleanup detected**: Several test files and vignettes were also identified as potentially archived

## Archive Manifest

A detailed manifest file `ARCHIVE_MANIFEST.md` has been created in the archive directory documenting:
- All archived files and their purposes
- Module organization
- Access instructions for future reference or restoration
- Git history preservation notes

## Next Steps (Recommended)

1. Review the deletion changes: `git diff --cached --stat`
2. Commit the archival: Create a git commit documenting this cleanup
3. Push to remote repository to synchronize
4. Optional: Archive additional test files and vignettes if desired

## Notes

- The reference directory location (`/Users/giampaolomontaletti/Documents/funzioni/reference/moneca/`) has been used as specified
- The `archived_R/` subdirectory was created within the moneca reference directory
- All files are readable and maintainable in their archived location
- Git history is preserved for all archived files at their original locations in the repository

