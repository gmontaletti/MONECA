#' Temporal Stability Analysis for MONECA Clustering
#'
#' Analyzes the stability and evolution of segments across time periods,
#' providing comprehensive metrics on segment persistence, node volatility,
#' and structural changes in the mobility network.
#'
#' @param temporal_result A moneca_temporal object returned by \code{\link{moneca_temporal}}.
#' @param stability_threshold Numeric threshold (0-1) for considering a segment stable.
#'   Default is 0.7 (70% of nodes remain).
#' @param min_segment_size Minimum number of nodes for a segment to be included in
#'   stability calculations. Default is 2.
#' @param compute_node_metrics Logical indicating whether to compute detailed
#'   node-level metrics. Default is TRUE.
#' @param verbose Logical indicating whether to show progress messages. Default is TRUE.
#'
#' @return An object of class "temporal_stability" containing:
#'   \describe{
#'     \item{stability_scores}{Data frame with per-segment and overall stability metrics}
#'     \item{node_trajectories}{Matrix showing the path of each node through segments}
#'     \item{change_points}{Vector of window indices with significant restructuring}
#'     \item{stable_core}{Character vector of nodes with consistent segment membership}
#'     \item{volatile_nodes}{Character vector of frequently changing nodes}
#'     \item{segment_lifetimes}{Data frame with segment appearance and persistence}
#'     \item{transition_summary}{Summary statistics of the transition matrix}
#'     \item{volatility_scores}{Per-node volatility scores}
#'     \item{parameters}{Analysis parameters used}
#'   }
#'
#' @details
#' The function computes several types of stability metrics:
#'
#' \strong{Segment-level metrics:}
#' \itemize{
#'   \item \strong{Persistence}: Proportion of windows where segment appears
#'   \item \strong{Stability}: Average proportion of nodes retained between consecutive windows
#'   \item \strong{Size variability}: Coefficient of variation in segment size
#'   \item \strong{Core size}: Number of nodes always in the segment
#' }
#'
#' \strong{Node-level metrics:}
#' \itemize{
#'   \item \strong{Volatility}: Frequency of segment changes
#'   \item \strong{Loyalty}: Proportion of time in most frequent segment
#'   \item \strong{Trajectory complexity}: Number of unique segments visited
#' }
#'
#' \strong{System-level metrics:}
#' \itemize{
#'   \item \strong{Overall stability}: Weighted average of segment stabilities
#'   \item \strong{Fragmentation index}: Rate of segment splits and merges
#'   \item \strong{Change points}: Windows with above-average restructuring
#' }
#'
#' @examples
#' \dontrun{
#' # Generate temporal data
#' months_data <- lapply(1:12, function(m) {
#'   generate_mobility_data(n_classes = 6, seed = m * 100)
#' })
#' 
#' # Run temporal analysis
#' temporal_result <- moneca_temporal(
#'   matrix_list = months_data,
#'   window_size = 3
#' )
#' 
#' # Analyze stability
#' stability <- temporal_stability_analysis(
#'   temporal_result,
#'   stability_threshold = 0.7,
#'   verbose = TRUE
#' )
#' 
#' # Print summary
#' print(stability)
#' 
#' # Access specific metrics
#' stable_nodes <- stability$stable_core
#' volatile_nodes <- stability$volatile_nodes
#' }
#'
#' @seealso 
#' \code{\link{moneca_temporal}} for temporal clustering,
#' \code{\link{plot_temporal_stability}} for visualization
#'
#' @export
temporal_stability_analysis <- function(temporal_result,
                                       stability_threshold = 0.7,
                                       min_segment_size = 2,
                                       compute_node_metrics = TRUE,
                                       verbose = TRUE) {
  
  if (!inherits(temporal_result, "moneca_temporal")) {
    stop("Input must be a moneca_temporal object")
  }
  
  if (verbose) {
    cat("Computing temporal stability metrics...\n")
  }
  
  # Extract data
  aligned_memberships <- temporal_result$stable_labels
  transition_matrix <- temporal_result$transition_matrix
  n_windows <- length(aligned_memberships)
  
  # Get all unique nodes and segments
  all_nodes <- unique(unlist(lapply(aligned_memberships, function(x) x$name)))
  all_segments <- unique(unlist(lapply(aligned_memberships, function(x) x$stable_label)))
  
  # Initialize results
  stability_scores <- data.frame(
    segment = all_segments,
    first_appearance = NA,
    last_appearance = NA,
    persistence = NA,
    avg_size = NA,
    size_cv = NA,
    stability = NA,
    core_size = NA,
    stringsAsFactors = FALSE
  )
  
  # Compute segment-level metrics
  if (verbose) cat("  Computing segment-level metrics...\n")
  
  for (seg in all_segments) {
    seg_idx <- which(stability_scores$segment == seg)
    
    # Find windows where segment appears
    appearances <- sapply(aligned_memberships, function(m) {
      seg %in% m$stable_label
    })
    
    if (sum(appearances) > 0) {
      window_indices <- which(appearances)
      stability_scores$first_appearance[seg_idx] <- min(window_indices)
      stability_scores$last_appearance[seg_idx] <- max(window_indices)
      stability_scores$persistence[seg_idx] <- sum(appearances) / n_windows
      
      # Get segment sizes across windows
      segment_sizes <- sapply(aligned_memberships[appearances], function(m) {
        sum(m$stable_label == seg)
      })
      
      if (length(segment_sizes) > 0) {
        stability_scores$avg_size[seg_idx] <- mean(segment_sizes)
        stability_scores$size_cv[seg_idx] <- if (mean(segment_sizes) > 0) {
          sd(segment_sizes) / mean(segment_sizes)
        } else 0
      }
      
      # Compute stability (node retention between consecutive appearances)
      if (length(window_indices) > 1) {
        retention_rates <- numeric()
        
        for (i in 2:length(window_indices)) {
          prev_window <- window_indices[i-1]
          curr_window <- window_indices[i]
          
          if (curr_window == prev_window + 1) {  # Consecutive windows
            prev_nodes <- aligned_memberships[[prev_window]]$name[
              aligned_memberships[[prev_window]]$stable_label == seg
            ]
            curr_nodes <- aligned_memberships[[curr_window]]$name[
              aligned_memberships[[curr_window]]$stable_label == seg
            ]
            
            if (length(prev_nodes) > 0) {
              retention <- length(intersect(prev_nodes, curr_nodes)) / length(prev_nodes)
              retention_rates <- c(retention_rates, retention)
            }
          }
        }
        
        if (length(retention_rates) > 0) {
          stability_scores$stability[seg_idx] <- mean(retention_rates)
        }
      } else {
        stability_scores$stability[seg_idx] <- 1  # Single appearance is stable
      }
      
      # Find core nodes (always in segment when it appears)
      nodes_in_segment <- lapply(aligned_memberships[appearances], function(m) {
        m$name[m$stable_label == seg]
      })
      
      if (length(nodes_in_segment) > 0) {
        core_nodes <- Reduce(intersect, nodes_in_segment)
        stability_scores$core_size[seg_idx] <- length(core_nodes)
      }
    }
  }
  
  # Node-level metrics
  node_trajectories <- NULL
  volatility_scores <- NULL
  stable_core <- NULL
  volatile_nodes <- NULL
  
  if (compute_node_metrics) {
    if (verbose) cat("  Computing node-level metrics...\n")
    
    # Create node trajectory matrix
    node_trajectories <- matrix(NA, 
                               nrow = length(all_nodes),
                               ncol = n_windows)
    rownames(node_trajectories) <- all_nodes
    colnames(node_trajectories) <- paste0("Window", 1:n_windows)
    
    # Fill trajectories
    for (w in 1:n_windows) {
      membership <- aligned_memberships[[w]]
      for (i in 1:nrow(membership)) {
        node <- membership$name[i]
        segment <- membership$stable_label[i]
        node_idx <- which(all_nodes == node)
        if (length(node_idx) > 0) {
          node_trajectories[node_idx, w] <- segment
        }
      }
    }
    
    # Compute volatility scores
    volatility_scores <- apply(node_trajectories, 1, function(trajectory) {
      valid_entries <- trajectory[!is.na(trajectory)]
      if (length(valid_entries) <= 1) return(0)
      
      # Count changes
      changes <- sum(valid_entries[-1] != valid_entries[-length(valid_entries)])
      volatility <- changes / (length(valid_entries) - 1)
      return(volatility)
    })
    
    # Identify stable core and volatile nodes
    stable_threshold <- 0.1  # Less than 10% changes
    volatile_threshold <- 0.5  # More than 50% changes
    
    stable_core <- names(volatility_scores)[volatility_scores <= stable_threshold]
    volatile_nodes <- names(volatility_scores)[volatility_scores >= volatile_threshold]
  }
  
  # Identify change points
  if (verbose) cat("  Identifying change points...\n")
  
  change_magnitudes <- numeric(n_windows - 1)
  for (w in 2:n_windows) {
    prev_membership <- aligned_memberships[[w-1]]
    curr_membership <- aligned_memberships[[w]]
    
    # Count nodes that changed segments
    common_nodes <- intersect(prev_membership$name, curr_membership$name)
    changes <- 0
    
    for (node in common_nodes) {
      prev_seg <- prev_membership$stable_label[prev_membership$name == node]
      curr_seg <- curr_membership$stable_label[curr_membership$name == node]
      
      if (length(prev_seg) > 0 && length(curr_seg) > 0) {
        if (prev_seg[1] != curr_seg[1]) {
          changes <- changes + 1
        }
      }
    }
    
    change_magnitudes[w-1] <- if (length(common_nodes) > 0) {
      changes / length(common_nodes)
    } else 0
  }
  
  # Change points are windows with above-average change
  mean_change <- mean(change_magnitudes)
  sd_change <- sd(change_magnitudes)
  change_threshold <- mean_change + sd_change
  change_points <- which(change_magnitudes > change_threshold) + 1
  
  # Segment lifetimes
  segment_lifetimes <- data.frame(
    segment = stability_scores$segment,
    birth_window = stability_scores$first_appearance,
    death_window = stability_scores$last_appearance,
    lifetime = stability_scores$last_appearance - stability_scores$first_appearance + 1,
    continuous = NA,
    stringsAsFactors = FALSE
  )
  
  # Check if segments are continuous
  for (i in 1:nrow(segment_lifetimes)) {
    seg <- segment_lifetimes$segment[i]
    appearances <- sapply(aligned_memberships, function(m) seg %in% m$stable_label)
    
    if (!is.na(segment_lifetimes$birth_window[i]) && !is.na(segment_lifetimes$death_window[i])) {
      expected_length <- segment_lifetimes$death_window[i] - segment_lifetimes$birth_window[i] + 1
      actual_length <- sum(appearances)
      segment_lifetimes$continuous[i] <- (expected_length == actual_length)
    }
  }
  
  # Transition summary
  transition_summary <- list(
    diagonal_weight = sum(diag(transition_matrix)),
    off_diagonal_weight = sum(transition_matrix) - sum(diag(transition_matrix)),
    max_transition = max(transition_matrix[row(transition_matrix) != col(transition_matrix)]),
    n_active_transitions = sum(transition_matrix > 0.01),
    entropy = -sum(transition_matrix[transition_matrix > 0] * 
                  log(transition_matrix[transition_matrix > 0]))
  )
  
  # Overall stability metrics
  overall_stability <- weighted.mean(
    stability_scores$stability[!is.na(stability_scores$stability)],
    stability_scores$avg_size[!is.na(stability_scores$stability)]
  )
  
  # Create result object
  result <- list(
    stability_scores = stability_scores,
    node_trajectories = node_trajectories,
    change_points = change_points,
    change_magnitudes = change_magnitudes,
    stable_core = stable_core,
    volatile_nodes = volatile_nodes,
    segment_lifetimes = segment_lifetimes,
    transition_summary = transition_summary,
    volatility_scores = volatility_scores,
    overall_stability = overall_stability,
    parameters = list(
      stability_threshold = stability_threshold,
      min_segment_size = min_segment_size,
      n_windows = n_windows,
      n_nodes = length(all_nodes),
      n_segments = length(all_segments)
    )
  )
  
  class(result) <- "temporal_stability"
  
  if (verbose) {
    cat("\nStability analysis complete!\n")
    cat(sprintf("  Overall stability: %.1f%%\n", overall_stability * 100))
    cat(sprintf("  Stable core nodes: %d (%.1f%%)\n", 
                length(stable_core), 
                100 * length(stable_core) / length(all_nodes)))
    cat(sprintf("  Volatile nodes: %d (%.1f%%)\n",
                length(volatile_nodes),
                100 * length(volatile_nodes) / length(all_nodes)))
    cat(sprintf("  Change points detected: %d\n", length(change_points)))
  }
  
  return(result)
}

#' Print Method for temporal_stability Objects
#'
#' @param x A temporal_stability object.
#' @param n_top Number of top segments to show. Default is 5.
#' @param ... Additional arguments (unused).
#'
#' @export
print.temporal_stability <- function(x, n_top = 5, ...) {
  cat("Temporal Stability Analysis Results\n")
  cat("===================================\n\n")
  
  cat("Summary Statistics:\n")
  cat(sprintf("  Analysis windows: %d\n", x$parameters$n_windows))
  cat(sprintf("  Total nodes: %d\n", x$parameters$n_nodes))
  cat(sprintf("  Total segments: %d\n", x$parameters$n_segments))
  cat(sprintf("  Overall stability: %.1f%%\n", x$overall_stability * 100))
  
  cat("\nNode Stability:\n")
  cat(sprintf("  Stable core: %d nodes (%.1f%%)\n", 
              length(x$stable_core),
              100 * length(x$stable_core) / x$parameters$n_nodes))
  cat(sprintf("  Volatile nodes: %d nodes (%.1f%%)\n",
              length(x$volatile_nodes),
              100 * length(x$volatile_nodes) / x$parameters$n_nodes))
  
  if (!is.null(x$volatility_scores)) {
    cat(sprintf("  Mean volatility: %.3f\n", mean(x$volatility_scores)))
    cat(sprintf("  Median volatility: %.3f\n", median(x$volatility_scores)))
  }
  
  cat("\nSegment Stability (Top", min(n_top, nrow(x$stability_scores)), "segments):\n")
  
  # Sort by stability and show top segments
  top_segments <- x$stability_scores[order(x$stability_scores$stability, 
                                          decreasing = TRUE), ]
  top_segments <- top_segments[!is.na(top_segments$stability), ]
  top_segments <- head(top_segments, n_top)
  
  if (nrow(top_segments) > 0) {
    for (i in 1:nrow(top_segments)) {
      cat(sprintf("  %s: stability=%.2f, persistence=%.2f, avg_size=%.1f\n",
                  top_segments$segment[i],
                  top_segments$stability[i],
                  top_segments$persistence[i],
                  top_segments$avg_size[i]))
    }
  }
  
  cat("\nChange Points:\n")
  if (length(x$change_points) > 0) {
    cat("  Windows with significant restructuring:", 
        paste(x$change_points, collapse = ", "), "\n")
    cat(sprintf("  Mean change magnitude: %.3f\n", mean(x$change_magnitudes)))
    cat(sprintf("  Max change magnitude: %.3f\n", max(x$change_magnitudes)))
  } else {
    cat("  No significant change points detected\n")
  }
  
  cat("\nTransition Dynamics:\n")
  if (!is.null(x$transition_summary)) {
    cat(sprintf("  Self-transition weight: %.2f\n", x$transition_summary$diagonal_weight))
    cat(sprintf("  Cross-transition weight: %.2f\n", x$transition_summary$off_diagonal_weight))
    cat(sprintf("  Active transitions: %d\n", x$transition_summary$n_active_transitions))
    cat(sprintf("  Transition entropy: %.2f\n", x$transition_summary$entropy))
  }
  
  invisible(x)
}

#' Export Node Trajectories to Data Frame
#'
#' Converts the node trajectory matrix to a long-format data frame suitable
#' for further analysis or visualization.
#'
#' @param stability_result A temporal_stability object.
#' @param include_volatility Logical indicating whether to include volatility scores.
#'   Default is TRUE.
#'
#' @return A data frame with columns: node, window, segment, and optionally volatility.
#'
#' @export
export_node_trajectories <- function(stability_result, include_volatility = TRUE) {
  
  if (!inherits(stability_result, "temporal_stability")) {
    stop("Input must be a temporal_stability object")
  }
  
  if (is.null(stability_result$node_trajectories)) {
    stop("No node trajectories found. Re-run analysis with compute_node_metrics = TRUE")
  }
  
  trajectories <- stability_result$node_trajectories
  n_nodes <- nrow(trajectories)
  n_windows <- ncol(trajectories)
  
  # Convert to long format
  result <- expand.grid(
    node = rownames(trajectories),
    window = 1:n_windows,
    stringsAsFactors = FALSE
  )
  
  result$segment <- as.vector(t(trajectories))
  
  if (include_volatility && !is.null(stability_result$volatility_scores)) {
    volatility_df <- data.frame(
      node = names(stability_result$volatility_scores),
      volatility = stability_result$volatility_scores,
      stringsAsFactors = FALSE
    )
    result <- merge(result, volatility_df, by = "node", all.x = TRUE)
  }
  
  return(result)
}