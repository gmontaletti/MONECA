#' Temporal Mobility Network Clustering Analysis
#'
#' Performs moving window clustering analysis on a time series of mobility matrices,
#' maintaining stable segment labels across time periods. This function extends MONECA
#' to handle temporal data by analyzing overlapping windows of mobility matrices and
#' tracking how segments evolve over time.
#'
#' @param matrix_list List of mobility matrices, one for each time period. Each matrix
#'   must be square with identical dimensions and row/column names.
#' @param window_size Integer specifying the number of time periods to include in each
#'   moving window. Default is 3.
#' @param segment.levels Integer specifying the maximum number of hierarchical 
#'   segmentation levels. Default is 3.
#' @param stability_method Character string specifying the method for matching segments
#'   across time periods:
#'   \itemize{
#'     \item "hungarian" (default): Uses Hungarian algorithm for optimal bipartite matching
#'     \item "jaccard": Uses Jaccard similarity of segment memberships
#'     \item "overlap": Simple overlap coefficient
#'   }
#' @param aggregation_method Character string specifying how to combine matrices within
#'   each window:
#'   \itemize{
#'     \item "mean" (default): Average of matrices in window
#'     \item "sum": Sum of matrices
#'     \item "weighted": Weighted average with more recent periods having higher weight
#'   }
#' @param cut.off Numeric threshold for minimum relative risk. Default is 1.
#' @param small.cell.reduction Numeric value for handling small cells. Default is 0.
#' @param min_overlap Minimum proportion of nodes that must overlap between segments
#'   for matching. Default is 0.5.
#' @param parallel Logical indicating whether to use parallel processing for windows.
#'   When TRUE, windows are processed in parallel using multiple CPU cores, which can
#'   significantly speed up analysis for large datasets. Uses mclapply on Unix/Mac
#'   and parLapply on Windows. Default is FALSE.
#' @param verbose Logical indicating whether to show progress messages. Default is TRUE.
#' @param ... Additional arguments passed to moneca().
#'
#' @return An object of class "moneca_temporal" containing:
#'   \describe{
#'     \item{windows}{List of time windows analyzed (start and end periods)}
#'     \item{segment_maps}{List of segment membership matrices for each window}
#'     \item{stable_labels}{Consistently labeled segments across all time periods}
#'     \item{transition_matrix}{Matrix showing segment-to-segment transitions}
#'     \item{moneca_results}{List of full moneca objects for each window}
#'     \item{matching_history}{Details of segment matching between windows}
#'     \item{parameters}{List of analysis parameters used}
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Creates overlapping windows of specified size from the input matrices
#' 2. Aggregates matrices within each window according to aggregation_method
#' 3. Runs moneca clustering on each aggregated window
#' 4. Matches segments between consecutive windows to maintain label stability
#' 5. Tracks transitions and changes in segment composition
#'
#' Segment matching uses either the Hungarian algorithm for optimal assignment
#' based on membership overlap, or Jaccard similarity for simpler matching.
#' The function ensures that segment labels remain as consistent as possible
#' across time periods, making it easier to track segment evolution.
#'
#' @examples
#' \dontrun{
#' # Generate synthetic monthly mobility data
#' months_data <- lapply(1:12, function(m) {
#'   generate_mobility_data(n_classes = 6, seed = m * 100)
#' })
#' 
#' # Run temporal analysis with 3-month moving window (sequential)
#' temporal_result <- moneca_temporal(
#'   matrix_list = months_data,
#'   window_size = 3,
#'   segment.levels = 3,
#'   stability_method = "hungarian",
#'   verbose = TRUE
#' )
#' 
#' # Run with parallel processing for faster execution
#' temporal_result_parallel <- moneca_temporal(
#'   matrix_list = months_data,
#'   window_size = 3,
#'   segment.levels = 3,
#'   stability_method = "hungarian",
#'   parallel = TRUE,  # Enable parallel processing
#'   verbose = TRUE
#' )
#' 
#' # Print summary
#' print(temporal_result)
#' 
#' # Analyze stability
#' stability <- temporal_stability_analysis(temporal_result)
#' }
#'
#' @seealso 
#' \code{\link{moneca}} for single-period analysis,
#' \code{\link{temporal_stability_analysis}} for stability metrics,
#' \code{\link{plot_temporal_segments}} for visualization
#'
#' @export
moneca_temporal <- function(matrix_list,
                          window_size = 3,
                          segment.levels = 3,
                          stability_method = "hungarian",
                          aggregation_method = "mean",
                          cut.off = 1,
                          small.cell.reduction = 0,
                          min_overlap = 0.5,
                          parallel = FALSE,
                          verbose = TRUE,
                          ...) {
  
  # Input validation
  if (!is.list(matrix_list)) {
    stop("matrix_list must be a list of matrices")
  }
  
  n_periods <- length(matrix_list)
  if (n_periods < window_size) {
    stop("Number of periods must be at least equal to window_size")
  }
  
  # Check matrix compatibility
  dims <- sapply(matrix_list, dim)
  if (length(unique(dims[1,])) > 1 || length(unique(dims[2,])) > 1) {
    stop("All matrices must have the same dimensions")
  }
  
  # Check row/column names consistency
  first_names <- rownames(matrix_list[[1]])
  for (i in 2:n_periods) {
    if (!identical(rownames(matrix_list[[i]]), first_names)) {
      warning(sprintf("Row names differ in matrix %d. Using names from first matrix.", i))
      rownames(matrix_list[[i]]) <- first_names
      colnames(matrix_list[[i]]) <- first_names
    }
  }
  
  if (!stability_method %in% c("hungarian", "jaccard", "overlap")) {
    stop("stability_method must be one of: 'hungarian', 'jaccard', 'overlap'")
  }
  
  if (!aggregation_method %in% c("mean", "sum", "weighted")) {
    stop("aggregation_method must be one of: 'mean', 'sum', 'weighted'")
  }
  
  if (verbose) {
    cat("Starting temporal MONECA analysis\n")
    cat(sprintf("- Time periods: %d\n", n_periods))
    cat(sprintf("- Window size: %d\n", window_size))
    cat(sprintf("- Number of windows: %d\n", n_periods - window_size + 1))
    cat(sprintf("- Stability method: %s\n", stability_method))
    cat("\n")
  }
  
  # Create windows
  n_windows <- n_periods - window_size + 1
  windows <- list()
  for (i in 1:n_windows) {
    windows[[i]] <- list(
      start = i,
      end = i + window_size - 1,
      periods = i:(i + window_size - 1)
    )
  }
  
  # Process each window
  moneca_results <- list()
  aggregated_matrices <- list()
  segment_maps <- list()
  
  if (parallel && n_windows > 1) {
    # Parallel processing
    if (verbose) {
      cat("Processing windows in parallel...\n")
      cat(sprintf("Using %d cores\n", parallel::detectCores() - 1))
    }
    
    # Define worker function for parallel processing
    process_window <- function(w) {
      window <- windows[[w]]
      
      # Aggregate matrices in window
      window_matrices <- matrix_list[window$periods]
      aggregated <- aggregate_mobility_window(
        window_matrices, 
        method = aggregation_method
      )
      
      # Run moneca on aggregated matrix
      moneca_result <- moneca(
        mx = aggregated,
        segment.levels = segment.levels,
        cut.off = cut.off,
        small.cell.reduction = small.cell.reduction,
        ...
      )
      
      # Extract segment membership at the optimal level
      available_levels <- length(moneca_result$segment.list)
      
      if (available_levels > 0) {
        # Find the optimal level with a reasonable number of segments
        # Start from the highest level and work down to find a level with 2-20 segments
        optimal_level <- available_levels  # Default to highest level
        max_reasonable_segments <- 20  # Maximum reasonable number of segments
        
        # Work from highest level down to find first level with reasonable segmentation
        for (level_check in available_levels:1) {
          n_segments_at_level <- length(moneca_result$segment.list[[level_check]])
          
          # Accept levels with 2-20 segments
          if (n_segments_at_level >= 2 && n_segments_at_level <= max_reasonable_segments) {
            optimal_level <- level_check
            break
          }
        }
        
        # If no level has reasonable segmentation, use the level closest to requested
        if (optimal_level == available_levels) {
          # Check if highest level has only 1 segment, then go to a reasonable lower level
          if (length(moneca_result$segment.list[[available_levels]]) <= 1) {
            # Find the level with segments closest to our target range (2-10 preferred)
            best_level <- available_levels
            best_score <- Inf
            
            for (level_check in available_levels:1) {
              n_segments <- length(moneca_result$segment.list[[level_check]])
              if (n_segments >= 2) {
                # Score based on how close to ideal range (2-10 segments)
                if (n_segments <= 10) {
                  score <- 0  # Perfect range
                } else {
                  score <- n_segments - 10  # Penalty for too many segments
                }
                
                if (score < best_score) {
                  best_score <- score
                  best_level <- level_check
                }
              }
            }
            optimal_level <- best_level
          }
        }
        
        membership <- segment.membership(moneca_result, level = optimal_level)
      } else {
        # If no segmentation occurred, create trivial membership
        n_nodes <- nrow(aggregated) - 1
        membership <- data.frame(
          name = rownames(aggregated)[1:n_nodes],
          membership = paste("1", 1:n_nodes, sep = ".")
        )
      }
      
      return(list(
        moneca_result = moneca_result,
        aggregated = aggregated,
        segment_map = membership
      ))
    }
    
    # Use mclapply for Unix/Mac or parLapply for Windows
    if (.Platform$OS.type == "unix") {
      # Unix/Mac - use mclapply
      n_cores <- min(parallel::detectCores() - 1, n_windows)
      results <- parallel::mclapply(
        1:n_windows, 
        process_window, 
        mc.cores = n_cores
      )
    } else {
      # Windows - use parLapply
      n_cores <- min(parallel::detectCores() - 1, n_windows)
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl))
      
      # Export necessary objects to cluster
      parallel::clusterExport(cl, c(
        "windows", "matrix_list", "aggregation_method",
        "segment.levels", "cut.off", "small.cell.reduction",
        "aggregate_mobility_window", "moneca", "segment.membership"
      ), envir = environment())
      
      results <- parallel::parLapply(cl, 1:n_windows, process_window)
    }
    
    # Extract results from parallel processing
    for (w in 1:n_windows) {
      moneca_results[[w]] <- results[[w]]$moneca_result
      aggregated_matrices[[w]] <- results[[w]]$aggregated
      segment_maps[[w]] <- results[[w]]$segment_map
    }
    
    if (verbose) {
      cat(sprintf("Processed %d windows in parallel\n", n_windows))
    }
    
  } else {
    # Sequential processing (original code)
    if (verbose) {
      cat("Processing windows:\n")
      pb <- txtProgressBar(min = 0, max = n_windows, style = 3)
    }
    
    for (w in 1:n_windows) {
      if (verbose) setTxtProgressBar(pb, w)
      
      window <- windows[[w]]
      
      # Aggregate matrices in window
      window_matrices <- matrix_list[window$periods]
      aggregated <- aggregate_mobility_window(
        window_matrices, 
        method = aggregation_method
      )
      aggregated_matrices[[w]] <- aggregated
      
      # Run moneca on aggregated matrix
      moneca_result <- moneca(
        mx = aggregated,
        segment.levels = segment.levels,
        cut.off = cut.off,
        small.cell.reduction = small.cell.reduction,
        ...
      )
      moneca_results[[w]] <- moneca_result
      
      # Extract segment membership at the optimal level
      available_levels <- length(moneca_result$segment.list)
      
      if (available_levels > 0) {
        # Find the optimal level with a reasonable number of segments
        # Start from the highest level and work down to find a level with 2-20 segments
        optimal_level <- available_levels  # Default to highest level
        max_reasonable_segments <- 20  # Maximum reasonable number of segments
        
        # Work from highest level down to find first level with reasonable segmentation
        for (level_check in available_levels:1) {
          n_segments_at_level <- length(moneca_result$segment.list[[level_check]])
          
          # Accept levels with 2-20 segments
          if (n_segments_at_level >= 2 && n_segments_at_level <= max_reasonable_segments) {
            optimal_level <- level_check
            break
          }
        }
        
        # If no level has reasonable segmentation, use the level closest to requested
        if (optimal_level == available_levels) {
          # Check if highest level has only 1 segment, then go to a reasonable lower level
          if (length(moneca_result$segment.list[[available_levels]]) <= 1) {
            # Find the level with segments closest to our target range (2-10 preferred)
            best_level <- available_levels
            best_score <- Inf
            
            for (level_check in available_levels:1) {
              n_segments <- length(moneca_result$segment.list[[level_check]])
              if (n_segments >= 2) {
                # Score based on how close to ideal range (2-10 segments)
                if (n_segments <= 10) {
                  score <- 0  # Perfect range
                } else {
                  score <- n_segments - 10  # Penalty for too many segments
                }
                
                if (score < best_score) {
                  best_score <- score
                  best_level <- level_check
                }
              }
            }
            optimal_level <- best_level
          }
        }
        
        membership <- segment.membership(moneca_result, level = optimal_level)
        segment_maps[[w]] <- membership
      } else {
        # If no segmentation occurred, create trivial membership
        n_nodes <- nrow(aggregated) - 1
        segment_maps[[w]] <- data.frame(
          name = rownames(aggregated)[1:n_nodes],
          membership = paste("1", 1:n_nodes, sep = ".")
        )
      }
    }
    
    if (verbose) {
      close(pb)
    }
  }
  
  if (verbose) {
    cat("\n\nMatching segments across windows...\n")
  }
  
  # Match segments across windows for stable labeling
  if (n_windows > 1) {
    stable_labels <- match_segments_across_time(
      segment_maps = segment_maps,
      method = stability_method,
      min_overlap = min_overlap,
      verbose = verbose
    )
  } else {
    # Single window case - no matching needed
    stable_labels <- list(
      aligned_memberships = segment_maps,
      mapping_history = list(),
      all_labels = unique(segment_maps[[1]]$membership)
    )
    # Add stable_label column
    stable_labels$aligned_memberships[[1]]$stable_label <- segment_maps[[1]]$membership
  }
  
  # Create transition matrix
  if (verbose) cat("Computing transition matrix...\n")
  if (n_windows > 1) {
    transition_matrix <- compute_transition_matrix(stable_labels$aligned_memberships)
  } else {
    # Single window case - create identity transition matrix
    all_segments <- unique(stable_labels$aligned_memberships[[1]]$stable_label)
    n_segments <- length(all_segments)
    transition_matrix <- diag(n_segments)
    rownames(transition_matrix) <- all_segments
    colnames(transition_matrix) <- all_segments
  }
  
  # Create output object
  result <- list(
    windows = windows,
    segment_maps = segment_maps,
    stable_labels = stable_labels$aligned_memberships,
    stable_mapping = stable_labels$mapping_history,
    transition_matrix = transition_matrix,
    moneca_results = moneca_results,
    aggregated_matrices = aggregated_matrices,
    parameters = list(
      n_periods = n_periods,
      window_size = window_size,
      segment.levels = segment.levels,
      stability_method = stability_method,
      aggregation_method = aggregation_method,
      cut.off = cut.off,
      small.cell.reduction = small.cell.reduction,
      min_overlap = min_overlap
    )
  )
  
  class(result) <- "moneca_temporal"
  
  if (verbose) {
    cat("\nTemporal analysis complete!\n")
    cat(sprintf("- Total segments identified: %d\n", 
                length(unique(unlist(lapply(stable_labels$aligned_memberships, 
                                           function(x) unique(x$stable_label)))))))
    cat(sprintf("- Average segments per window: %.1f\n",
                mean(sapply(stable_labels$aligned_memberships, 
                          function(x) length(unique(x$stable_label))))))
  }
  
  return(result)
}

#' Aggregate Mobility Matrices in Window
#'
#' Internal function to combine multiple mobility matrices within a time window.
#'
#' @param matrices List of matrices to aggregate.
#' @param method Aggregation method: "mean", "sum", or "weighted".
#'
#' @return Aggregated matrix.
#' @keywords internal
aggregate_mobility_window <- function(matrices, method = "mean") {
  
  n_matrices <- length(matrices)
  
  # Check if input matrices have margins and remove them for aggregation
  has_margins <- FALSE
  if (!is.null(matrices[[1]])) {
    n_dim <- nrow(matrices[[1]])
    # Check if last row and column are likely margin sums
    # (they would typically be named "Total" or be unnamed, and have larger values)
    last_row_name <- rownames(matrices[[1]])[n_dim]
    last_col_name <- colnames(matrices[[1]])[n_dim]
    
    # If matrices have margins, work with the core data only
    if (identical(last_row_name, "Total") || identical(last_col_name, "Total") ||
        is.null(last_row_name) || last_row_name == as.character(n_dim)) {
      has_margins <- TRUE
      # Extract core matrices without margins for aggregation
      core_matrices <- lapply(matrices, function(m) {
        m[1:(n_dim-1), 1:(n_dim-1)]
      })
    } else {
      core_matrices <- matrices
    }
  } else {
    core_matrices <- matrices
  }
  
  if (method == "mean") {
    # Simple average
    result <- Reduce("+", core_matrices) / n_matrices
    
  } else if (method == "sum") {
    # Sum all matrices
    result <- Reduce("+", core_matrices)
    
  } else if (method == "weighted") {
    # Weighted average with more recent periods having higher weight
    weights <- seq(0.5, 1, length.out = n_matrices)
    weights <- weights / sum(weights)
    
    # VECTORIZED WEIGHTED MATRIX COMBINATION
    # Replace sequential loop with vectorized matrix operations
    weighted_matrices <- Map(`*`, core_matrices, weights)
    result <- Reduce(`+`, weighted_matrices)
  }
  
  # Add margin sums (row and column totals) to the aggregated matrix
  # This is required for moneca() to work properly
  n_rows <- nrow(result)
  n_cols <- ncol(result)
  
  # Calculate row sums
  row_sums <- rowSums(result, na.rm = TRUE)
  # Calculate column sums
  col_sums <- colSums(result, na.rm = TRUE)
  # Calculate total
  total_sum <- sum(result, na.rm = TRUE)
  
  # Create matrix with margins
  result_with_margins <- matrix(0, nrow = n_rows + 1, ncol = n_cols + 1)
  
  # Copy the original data
  result_with_margins[1:n_rows, 1:n_cols] <- result
  
  # Add row sums
  result_with_margins[1:n_rows, n_cols + 1] <- row_sums
  
  # Add column sums
  result_with_margins[n_rows + 1, 1:n_cols] <- col_sums
  
  # Add total
  result_with_margins[n_rows + 1, n_cols + 1] <- total_sum
  
  # Preserve row and column names
  if (!is.null(rownames(result))) {
    rownames(result_with_margins) <- c(rownames(result), "Total")
  }
  if (!is.null(colnames(result))) {
    colnames(result_with_margins) <- c(colnames(result), "Total")
  }
  
  return(result_with_margins)
}

#' Match Segments Across Time Windows
#'
#' Internal function to maintain stable segment labels across time periods.
#'
#' @param segment_maps List of segment membership data frames.
#' @param method Matching method: "hungarian", "jaccard", or "overlap".
#' @param min_overlap Minimum overlap proportion for matching.
#' @param verbose Show progress messages.
#'
#' @return List with aligned memberships and mapping history.
#' @keywords internal
match_segments_across_time <- function(segment_maps, 
                                      method = "hungarian",
                                      min_overlap = 0.5,
                                      verbose = FALSE) {
  
  n_windows <- length(segment_maps)
  aligned_memberships <- list()
  mapping_history <- list()
  
  # Initialize first window with original labels
  aligned_memberships[[1]] <- segment_maps[[1]]
  aligned_memberships[[1]]$stable_label <- segment_maps[[1]]$membership
  
  # Track all unique labels seen
  all_labels <- unique(aligned_memberships[[1]]$stable_label)
  next_label_id <- length(all_labels) + 1
  
  # Match each subsequent window to the previous one
  for (w in 2:n_windows) {
    if (verbose) cat(sprintf("  Matching window %d to window %d...\n", w, w-1))
    
    prev_membership <- aligned_memberships[[w-1]]
    curr_membership <- segment_maps[[w]]
    
    # Get unique segments in each window
    prev_segments <- unique(prev_membership$stable_label)
    curr_segments <- unique(curr_membership$membership)
    
    # Create similarity matrix
    similarity_matrix <- matrix(0, 
                               nrow = length(prev_segments),
                               ncol = length(curr_segments))
    rownames(similarity_matrix) <- prev_segments
    colnames(similarity_matrix) <- curr_segments
    
    # VECTORIZED SIMILARITY CALCULATION - Major performance optimization
    # Replace nested loops with vectorized operations using outer/mapply
    
    # Pre-compute node sets for all segments (vectorized)
    prev_node_sets <- lapply(prev_segments, function(seg) {
      prev_membership$name[prev_membership$stable_label == seg]
    })
    names(prev_node_sets) <- prev_segments
    
    curr_node_sets <- lapply(curr_segments, function(seg) {
      curr_membership$name[curr_membership$membership == seg]
    })
    names(curr_node_sets) <- curr_segments
    
    # Vectorized similarity computation using outer
    if (method == "jaccard") {
      # Jaccard similarity using vectorized operations
      similarity_matrix <- outer(seq_along(prev_segments), seq_along(curr_segments), 
                                FUN = Vectorize(function(i, j) {
                                  prev_nodes <- prev_node_sets[[i]]
                                  curr_nodes <- curr_node_sets[[j]]
                                  
                                  intersection_size <- length(intersect(prev_nodes, curr_nodes))
                                  union_size <- length(union(prev_nodes, curr_nodes))
                                  
                                  if (union_size > 0) intersection_size / union_size else 0
                                }))
      
    } else if (method == "overlap") {
      # Overlap coefficient using vectorized operations
      similarity_matrix <- outer(seq_along(prev_segments), seq_along(curr_segments), 
                                FUN = Vectorize(function(i, j) {
                                  prev_nodes <- prev_node_sets[[i]]
                                  curr_nodes <- curr_node_sets[[j]]
                                  
                                  intersection_size <- length(intersect(prev_nodes, curr_nodes))
                                  min_size <- min(length(prev_nodes), length(curr_nodes))
                                  
                                  if (min_size > 0) intersection_size / min_size else 0
                                }))
      
    } else {  # hungarian
      # Vectorized overlap computation for Hungarian algorithm
      similarity_matrix <- outer(seq_along(prev_segments), seq_along(curr_segments), 
                                FUN = Vectorize(function(i, j) {
                                  prev_nodes <- prev_node_sets[[i]]
                                  curr_nodes <- curr_node_sets[[j]]
                                  
                                  length(intersect(prev_nodes, curr_nodes))
                                }))
    }
    
    # Set row and column names for the vectorized result
    rownames(similarity_matrix) <- prev_segments
    colnames(similarity_matrix) <- curr_segments
    
    # Find optimal matching
    if (method == "hungarian" && requireNamespace("clue", quietly = TRUE)) {
      # Use Hungarian algorithm for optimal bipartite matching
      # Convert to cost matrix (maximize similarity = minimize negative similarity)
      cost_matrix <- -similarity_matrix
      assignment <- clue::solve_LSAP(cost_matrix, maximum = FALSE)
      
      mapping <- data.frame(
        from = curr_segments,
        to = NA,
        similarity = NA
      )
      
      for (j in seq_along(curr_segments)) {
        matched_idx <- which(assignment == j)
        if (length(matched_idx) > 0) {
          sim_value <- similarity_matrix[matched_idx, j]
          # Check if similarity meets threshold
          total_nodes <- nrow(curr_membership)
          if (sim_value / total_nodes >= min_overlap) {
            mapping$to[j] <- prev_segments[matched_idx]
            mapping$similarity[j] <- sim_value
          }
        }
      }
      
    } else {
      # Greedy matching for other methods
      mapping <- data.frame(
        from = curr_segments,
        to = NA,
        similarity = NA
      )
      
      # Sort by maximum similarity
      for (j in seq_along(curr_segments)) {
        max_sim_idx <- which.max(similarity_matrix[, j])
        max_sim <- similarity_matrix[max_sim_idx, j]
        
        if (max_sim >= min_overlap) {
          mapping$to[j] <- prev_segments[max_sim_idx]
          mapping$similarity[j] <- max_sim
          # Remove matched row to prevent double assignment
          similarity_matrix[max_sim_idx, ] <- -1
        }
      }
    }
    
    # Apply mapping to create stable labels
    aligned_memberships[[w]] <- curr_membership
    aligned_memberships[[w]]$stable_label <- NA
    
    for (j in seq_along(curr_segments)) {
      curr_seg <- curr_segments[j]
      rows_to_update <- which(curr_membership$membership == curr_seg)
      
      if (!is.na(mapping$to[j])) {
        # Use existing label from previous window
        aligned_memberships[[w]]$stable_label[rows_to_update] <- mapping$to[j]
      } else {
        # Create new label for unmatched segment
        new_label <- sprintf("T%d.%d", w, next_label_id)
        aligned_memberships[[w]]$stable_label[rows_to_update] <- new_label
        all_labels <- c(all_labels, new_label)
        next_label_id <- next_label_id + 1
      }
    }
    
    mapping_history[[w-1]] <- mapping
  }
  
  return(list(
    aligned_memberships = aligned_memberships,
    mapping_history = mapping_history,
    all_labels = all_labels
  ))
}

#' Compute Transition Matrix
#'
#' Internal function to compute segment-to-segment transition probabilities.
#'
#' @param aligned_memberships List of aligned membership data frames.
#'
#' @return Transition probability matrix.
#' @keywords internal
compute_transition_matrix <- function(aligned_memberships) {
  
  n_windows <- length(aligned_memberships)
  
  # Get all unique segment labels
  all_segments <- unique(unlist(lapply(aligned_memberships, 
                                      function(x) unique(x$stable_label))))
  n_segments <- length(all_segments)
  
  # Initialize transition count matrix
  transition_counts <- matrix(0, 
                             nrow = n_segments,
                             ncol = n_segments)
  rownames(transition_counts) <- all_segments
  colnames(transition_counts) <- all_segments
  
  # VECTORIZED TRANSITION COUNTING - Major performance optimization
  # Replace nested loops with vectorized operations
  
  for (w in 2:n_windows) {
    prev_membership <- aligned_memberships[[w-1]]
    curr_membership <- aligned_memberships[[w]]
    
    # Vectorized node matching and label extraction
    common_nodes <- intersect(prev_membership$name, curr_membership$name)
    
    if (length(common_nodes) > 0) {
      # Vectorized label extraction using match
      prev_indices_in_df <- match(common_nodes, prev_membership$name)
      curr_indices_in_df <- match(common_nodes, curr_membership$name)
      
      # Extract labels for all common nodes at once
      prev_labels <- prev_membership$stable_label[prev_indices_in_df]
      curr_labels <- curr_membership$stable_label[curr_indices_in_df]
      
      # Remove any nodes with missing labels (vectorized filtering)
      valid_transitions <- !is.na(prev_labels) & !is.na(curr_labels)
      prev_labels <- prev_labels[valid_transitions]
      curr_labels <- curr_labels[valid_transitions]
      
      if (length(prev_labels) > 0) {
        # Vectorized index mapping for segment labels
        prev_seg_indices <- match(prev_labels, all_segments)
        curr_seg_indices <- match(curr_labels, all_segments)
        
        # Filter out any unmatched segments
        valid_matches <- !is.na(prev_seg_indices) & !is.na(curr_seg_indices)
        prev_seg_indices <- prev_seg_indices[valid_matches]
        curr_seg_indices <- curr_seg_indices[valid_matches]
        
        # Vectorized transition counting using table
        if (length(prev_seg_indices) > 0) {
          # Create transition pairs and count efficiently
          transition_pairs <- data.frame(
            from = prev_seg_indices,
            to = curr_seg_indices
          )
          
          # Use table to count transitions efficiently
          transition_table <- table(transition_pairs$from, transition_pairs$to)
          
          # Add to main transition matrix (vectorized addition)
          for (i in seq_len(nrow(transition_table))) {
            for (j in seq_len(ncol(transition_table))) {
              from_idx <- as.numeric(rownames(transition_table)[i])
              to_idx <- as.numeric(colnames(transition_table)[j])
              if (from_idx <= n_segments && to_idx <= n_segments) {
                transition_counts[from_idx, to_idx] <- 
                  transition_counts[from_idx, to_idx] + transition_table[i, j]
              }
            }
          }
        }
      }
    }
  }
  
  # Convert to probabilities
  row_sums <- rowSums(transition_counts)
  transition_probs <- transition_counts
  
  for (i in 1:n_segments) {
    if (row_sums[i] > 0) {
      transition_probs[i, ] <- transition_counts[i, ] / row_sums[i]
    }
  }
  
  return(transition_probs)
}

#' Print Method for moneca_temporal Objects
#'
#' @param x A moneca_temporal object.
#' @param ... Additional arguments (unused).
#'
#' @export
print.moneca_temporal <- function(x, ...) {
  cat("Temporal MONECA Analysis Results\n")
  cat("================================\n\n")
  
  cat("Analysis Parameters:\n")
  cat(sprintf("  Time periods: %d\n", x$parameters$n_periods))
  cat(sprintf("  Window size: %d\n", x$parameters$window_size))
  cat(sprintf("  Number of windows: %d\n", length(x$windows)))
  cat(sprintf("  Segmentation levels: %d\n", x$parameters$segment.levels))
  cat(sprintf("  Stability method: %s\n", x$parameters$stability_method))
  cat(sprintf("  Aggregation method: %s\n", x$parameters$aggregation_method))
  
  cat("\nSegmentation Summary:\n")
  
  # Count unique segments per window
  segments_per_window <- sapply(x$stable_labels, 
                               function(m) length(unique(m$stable_label)))
  
  cat(sprintf("  Total unique segments: %d\n", 
              length(unique(unlist(lapply(x$stable_labels, 
                                        function(m) unique(m$stable_label)))))))
  cat(sprintf("  Segments per window: min=%d, mean=%.1f, max=%d\n",
              min(segments_per_window),
              mean(segments_per_window),
              max(segments_per_window)))
  
  # Calculate stability
  if (length(x$windows) > 1) {
    total_transitions <- sum(x$transition_matrix[row(x$transition_matrix) != 
                                                col(x$transition_matrix)])
    total_stable <- sum(diag(x$transition_matrix))
    
    if ((total_transitions + total_stable) > 0) {
      stability_rate <- total_stable / (total_transitions + total_stable)
      cat(sprintf("  Overall stability rate: %.1f%%\n", stability_rate * 100))
    }
  }
  
  cat("\nUse temporal_stability_analysis() for detailed stability metrics\n")
  cat("Use plot_temporal_segments() for visualization\n")
  
  invisible(x)
}