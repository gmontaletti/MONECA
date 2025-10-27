#' Visualize Temporal Segment Evolution
#'
#' Creates various visualizations for temporal clustering results, including
#' alluvial diagrams, heatmaps, and timeline plots showing how segments evolve
#' over time.
#'
#' @param temporal_result A moneca_temporal object or temporal_stability object.
#' @param type Character string specifying the plot type:
#'   \itemize{
#'     \item "alluvial": Alluvial/Sankey diagram of segment transitions
#'     \item "heatmap": Heatmap of transition probabilities
#'     \item "timeline": Timeline showing segment sizes over time
#'     \item "stability": Stability metrics over time
#'   }
#' @param min_flow Minimum flow proportion to display in alluvial diagram. Default is 0.05.
#' @param color_scheme Character string specifying color scheme: "default", "viridis",
#'   "RdYlBu", or "Set3". Default is "default".
#' @param show_labels Logical indicating whether to show segment labels. Default is TRUE.
#' @param title Character string for plot title. If NULL, auto-generated based on type.
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A ggplot2 object that can be further customized or saved.
#'
#' @details
#' The function creates different visualizations based on the selected type:
#'
#' \strong{Alluvial diagram}: Shows flow of nodes between segments across time windows.
#' Wider ribbons indicate more nodes following that transition path.
#'
#' \strong{Heatmap}: Displays the transition probability matrix with color intensity
#' representing transition likelihood.
#'
#' \strong{Timeline}: Line plot showing how segment sizes change over time, useful
#' for identifying growing, shrinking, or stable segments.
#'
#' \strong{Stability}: Multiple panels showing stability metrics evolution including
#' overall stability, number of active segments, and change magnitudes.
#'
#' @examples
#' \dontrun{
#' # Generate and analyze temporal data
#' months_data <- lapply(1:12, function(m) {
#'   generate_mobility_data(n_classes = 6, seed = m * 100)
#' })
#' 
#' temporal_result <- moneca_temporal(months_data, window_size = 3)
#' stability <- temporal_stability_analysis(temporal_result)
#' 
#' # Create alluvial diagram
#' plot_temporal_segments(temporal_result, type = "alluvial")
#' 
#' # Create heatmap of transitions
#' plot_temporal_segments(temporal_result, type = "heatmap")
#' 
#' # Show timeline of segment sizes
#' plot_temporal_segments(temporal_result, type = "timeline")
#' 
#' # Plot stability metrics
#' plot_temporal_segments(stability, type = "stability")
#' }
#'
#' @seealso 
#' \code{\link{moneca_temporal}} for temporal clustering,
#' \code{\link{temporal_stability_analysis}} for stability metrics
#'
#' @import ggplot2
#' @importFrom scales percent_format
#' @export
plot_temporal_segments <- function(temporal_result,
                                  type = "alluvial",
                                  min_flow = 0.05,
                                  color_scheme = "default",
                                  show_labels = TRUE,
                                  title = NULL,
                                  ...) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }
  
  # Determine input type
  if (inherits(temporal_result, "moneca_temporal")) {
    temporal_data <- temporal_result
    stability_data <- NULL
  } else if (inherits(temporal_result, "temporal_stability")) {
    stability_data <- temporal_result
    temporal_data <- NULL
  } else {
    stop("Input must be a moneca_temporal or temporal_stability object")
  }
  
  # Select plot based on type
  if (type == "alluvial") {
    if (is.null(temporal_data)) {
      stop("Alluvial plot requires a moneca_temporal object")
    }
    p <- plot_alluvial_diagram(temporal_data, min_flow, color_scheme, show_labels)
    
  } else if (type == "heatmap") {
    if (is.null(temporal_data)) {
      stop("Heatmap requires a moneca_temporal object")
    }
    p <- plot_transition_heatmap(temporal_data, color_scheme, show_labels)
    
  } else if (type == "timeline") {
    if (is.null(temporal_data)) {
      stop("Timeline plot requires a moneca_temporal object")
    }
    p <- plot_segment_timeline(temporal_data, color_scheme, show_labels)
    
  } else if (type == "stability") {
    if (is.null(stability_data)) {
      stop("Stability plot requires a temporal_stability object")
    }
    p <- plot_stability_metrics(stability_data, color_scheme)
    
  } else {
    stop("Plot type must be one of: 'alluvial', 'heatmap', 'timeline', 'stability'")
  }
  
  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  
  return(p)
}

#' Create Alluvial Diagram for Segment Transitions
#'
#' Internal function to create an alluvial/Sankey diagram.
#'
#' @keywords internal
plot_alluvial_diagram <- function(temporal_data, min_flow, color_scheme, show_labels) {
  
  # Check for ggalluvial package
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("Package 'ggalluvial' is required for alluvial plots. Install with: install.packages('ggalluvial')")
  }
  
  # Validate input data
  if (is.null(temporal_data$stable_labels) || length(temporal_data$stable_labels) == 0) {
    stop("No stable labels found in temporal data")
  }
  
  # Prepare data for alluvial plot
  aligned_memberships <- temporal_data$stable_labels
  n_windows <- length(aligned_memberships)
  
  # Validate window data
  if (n_windows < 2) {
    stop("At least 2 time windows are required for alluvial diagram")
  }
  
  # Create long format data with better error handling
  alluvial_data <- NULL
  total_nodes <- 0
  
  for (w in 1:n_windows) {
    window_data <- aligned_memberships[[w]]
    
    # Validate window data structure
    if (is.null(window_data) || nrow(window_data) == 0) {
      warning(paste("Window", w, "contains no data, skipping"))
      next
    }
    
    # Ensure required columns exist
    if (!all(c("name", "stable_label") %in% names(window_data))) {
      stop(paste("Window", w, "missing required columns: name, stable_label"))
    }
    
    window_data$window <- w
    window_subset <- window_data[, c("name", "stable_label", "window")]
    
    # Remove rows with missing values
    window_subset <- window_subset[complete.cases(window_subset), ]
    
    if (nrow(window_subset) == 0) {
      warning(paste("Window", w, "contains no complete cases, skipping"))
      next
    }
    
    if (is.null(alluvial_data)) {
      alluvial_data <- window_subset
      total_nodes <- length(unique(window_subset$name))
    } else {
      alluvial_data <- rbind(alluvial_data, window_subset)
    }
  }
  
  # Validate final data
  if (is.null(alluvial_data) || nrow(alluvial_data) == 0) {
    stop("No valid data found across all windows")
  }
  
  # Check data size to prevent memory issues
  if (nrow(alluvial_data) > 50000) {
    warning("Large dataset detected. Consider using min_flow to filter small flows.")
  }
  
  # Apply minimum flow filtering at the node level
  if (min_flow > 0) {
    # Count how many times each node appears
    node_counts <- table(alluvial_data$name)
    min_appearances <- max(1, floor(total_nodes * min_flow))
    
    # Keep only nodes that appear frequently enough
    frequent_nodes <- names(node_counts)[node_counts >= min_appearances]
    
    if (length(frequent_nodes) > 0) {
      alluvial_data <- alluvial_data[alluvial_data$name %in% frequent_nodes, ]
    }
  }
  
  # Ensure we still have data after filtering
  if (nrow(alluvial_data) == 0) {
    stop("No data remaining after applying min_flow filter")
  }
  
  # Create color palette
  all_segments <- unique(alluvial_data$stable_label)
  n_segments <- length(all_segments)
  
  if (n_segments == 0) {
    stop("No segments found in data")
  }
  
  # Handle color schemes with proper error checking
  tryCatch({
    if (color_scheme == "viridis") {
      if (!requireNamespace("viridis", quietly = TRUE)) {
        warning("viridis package not available, using default colors")
        colors <- scales::hue_pal()(n_segments)
      } else {
        colors <- viridis::viridis(n_segments)
      }
    } else if (color_scheme == "RdYlBu") {
      if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
        warning("RColorBrewer package not available, using default colors")
        colors <- scales::hue_pal()(n_segments)
      } else {
        colors <- RColorBrewer::brewer.pal(min(max(3, n_segments), 11), "RdYlBu")
        if (n_segments > 11) {
          colors <- colorRampPalette(colors)(n_segments)
        }
      }
    } else if (color_scheme == "Set3") {
      if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
        warning("RColorBrewer package not available, using default colors")
        colors <- scales::hue_pal()(n_segments)
      } else {
        colors <- RColorBrewer::brewer.pal(min(max(3, n_segments), 12), "Set3")
        if (n_segments > 12) {
          colors <- colorRampPalette(colors)(n_segments)
        }
      }
    } else {
      colors <- scales::hue_pal()(n_segments)
    }
  }, error = function(e) {
    warning(paste("Error in color palette generation:", e$message, ". Using default colors."))
    colors <- scales::hue_pal()(n_segments)
  })
  
  names(colors) <- all_segments
  
  # Create plot with error handling
  tryCatch({
    p <- ggplot2::ggplot(alluvial_data,
                         ggplot2::aes(x = window, 
                                     stratum = stable_label,
                                     alluvium = name,
                                     fill = stable_label)) +
      ggalluvial::geom_flow(alpha = 0.6) +
      ggalluvial::geom_stratum(alpha = 0.8) +
      ggplot2::scale_fill_manual(values = colors, name = "Segment") +
      ggplot2::scale_x_continuous(breaks = 1:n_windows,
                                 labels = paste0("W", 1:n_windows)) +
      ggplot2::labs(x = "Time Window",
                   y = "Number of Nodes",
                   title = "Segment Evolution Over Time") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "right")
    
    if (show_labels) {
      p <- p + ggplot2::geom_text(stat = ggalluvial::StatStratum,
                                  ggplot2::aes(label = stable_label),
                                  size = 3)
    }
    
    return(p)
    
  }, error = function(e) {
    stop(paste("Error creating alluvial plot:", e$message))
  })
}

#' Create Transition Probability Heatmap
#'
#' Internal function to create a heatmap of transition probabilities.
#'
#' @keywords internal
plot_transition_heatmap <- function(temporal_data, color_scheme, show_labels) {
  
  transition_matrix <- temporal_data$transition_matrix
  
  # Convert to long format for ggplot
  n_segments <- nrow(transition_matrix)
  heatmap_data <- expand.grid(
    from = rownames(transition_matrix),
    to = colnames(transition_matrix)
  )
  heatmap_data$probability <- as.vector(transition_matrix)
  
  # Create color scale
  if (color_scheme == "viridis") {
    color_scale <- ggplot2::scale_fill_viridis_c()
  } else if (color_scheme == "RdYlBu") {
    color_scale <- ggplot2::scale_fill_distiller(palette = "RdYlBu")
  } else {
    color_scale <- ggplot2::scale_fill_gradient(low = "white", high = "darkblue")
  }
  
  # Create plot
  p <- ggplot2::ggplot(heatmap_data,
                       ggplot2::aes(x = to, y = from, fill = probability)) +
    ggplot2::geom_tile() +
    color_scale +
    ggplot2::labs(x = "To Segment",
                 y = "From Segment",
                 fill = "Probability",
                 title = "Segment Transition Probabilities") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (show_labels) {
    # Add text labels for high probabilities
    high_prob_data <- heatmap_data[heatmap_data$probability > 0.1, ]
    if (nrow(high_prob_data) > 0) {
      p <- p + ggplot2::geom_text(data = high_prob_data,
                                 ggplot2::aes(label = sprintf("%.2f", probability)),
                                 size = 3)
    }
  }
  
  return(p)
}

#' Create Segment Size Timeline
#'
#' Internal function to create a timeline of segment sizes.
#'
#' @keywords internal
plot_segment_timeline <- function(temporal_data, color_scheme, show_labels) {
  
  aligned_memberships <- temporal_data$stable_labels
  n_windows <- length(aligned_memberships)
  
  # Calculate segment sizes for each window
  timeline_data <- NULL
  
  for (w in 1:n_windows) {
    segment_counts <- table(aligned_memberships[[w]]$stable_label)
    window_summary <- data.frame(
      window = w,
      segment = names(segment_counts),
      size = as.numeric(segment_counts),
      stringsAsFactors = FALSE
    )
    
    if (is.null(timeline_data)) {
      timeline_data <- window_summary
    } else {
      timeline_data <- rbind(timeline_data, window_summary)
    }
  }
  
  # Create color palette
  all_segments <- unique(timeline_data$segment)
  n_segments <- length(all_segments)
  
  if (color_scheme == "viridis") {
    colors <- viridis::viridis(n_segments)
  } else if (color_scheme == "Set3") {
    colors <- RColorBrewer::brewer.pal(min(n_segments, 12), "Set3")
  } else {
    colors <- scales::hue_pal()(n_segments)
  }
  
  names(colors) <- all_segments
  
  # Create plot
  p <- ggplot2::ggplot(timeline_data,
                       ggplot2::aes(x = window, y = size, 
                                   color = segment, group = segment)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_color_manual(values = colors, name = "Segment") +
    ggplot2::scale_x_continuous(breaks = 1:n_windows,
                               labels = paste0("W", 1:n_windows)) +
    ggplot2::labs(x = "Time Window",
                 y = "Segment Size",
                 title = "Segment Size Evolution") +
    ggplot2::theme_minimal()
  
  if (show_labels) {
    # Add labels at the end of each line
    last_window_data <- timeline_data[timeline_data$window == n_windows, ]
    p <- p + ggplot2::geom_text(data = last_window_data,
                               ggplot2::aes(label = segment),
                               hjust = -0.1,
                               size = 3)
  }
  
  return(p)
}

#' Create Stability Metrics Plot
#'
#' Internal function to create stability metrics visualization.
#'
#' @keywords internal
plot_stability_metrics <- function(stability_data, color_scheme) {
  
  n_windows <- stability_data$parameters$n_windows
  
  # Prepare data for multi-panel plot
  metrics_data <- data.frame(
    window = 2:n_windows,
    change_magnitude = stability_data$change_magnitudes,
    stringsAsFactors = FALSE
  )
  
  # Add segment count per window if available
  if (!is.null(stability_data$node_trajectories)) {
    segment_counts <- apply(stability_data$node_trajectories, 2, function(col) {
      length(unique(col[!is.na(col)]))
    })
    metrics_data$n_segments <- segment_counts[-1]
  }
  
  # Create base plot for change magnitudes
  p1 <- ggplot2::ggplot(metrics_data,
                        ggplot2::aes(x = window, y = change_magnitude)) +
    ggplot2::geom_line(color = "darkred", linewidth = 1) +
    ggplot2::geom_point(color = "darkred", size = 2) +
    ggplot2::geom_hline(yintercept = mean(metrics_data$change_magnitude),
                       linetype = "dashed", color = "gray50") +
    ggplot2::labs(x = "Time Window",
                 y = "Change Magnitude",
                 title = "Structural Changes Over Time") +
    ggplot2::theme_minimal()
  
  # Mark change points
  if (length(stability_data$change_points) > 0) {
    change_point_data <- data.frame(
      window = stability_data$change_points,
      change_magnitude = metrics_data$change_magnitude[
        stability_data$change_points - 1
      ]
    )
    p1 <- p1 + ggplot2::geom_point(data = change_point_data,
                                  color = "red", size = 4, shape = 17)
  }
  
  # If we have segment counts, create second panel
  if ("n_segments" %in% names(metrics_data)) {
    p2 <- ggplot2::ggplot(metrics_data,
                         ggplot2::aes(x = window, y = n_segments)) +
      ggplot2::geom_line(color = "darkblue", linewidth = 1) +
      ggplot2::geom_point(color = "darkblue", size = 2) +
      ggplot2::labs(x = "Time Window",
                   y = "Number of Segments",
                   title = "Active Segments Over Time") +
      ggplot2::theme_minimal()
    
    # Combine plots
    if (requireNamespace("patchwork", quietly = TRUE)) {
      p <- p1 / p2
    } else {
      p <- p1
      message("Install 'patchwork' package for multi-panel plots")
    }
  } else {
    p <- p1
  }
  
  return(p)
}

#' Create Summary Report for Temporal Analysis
#'
#' Generates a comprehensive HTML or PDF report summarizing temporal clustering
#' results and stability analysis.
#'
#' @param temporal_result A moneca_temporal object.
#' @param stability_result A temporal_stability object (optional).
#' @param output_file Character string specifying output file path.
#' @param format Character string: "html" or "pdf". Default is "html".
#' @param include_plots Logical indicating whether to include visualizations.
#'   Default is TRUE.
#'
#' @export
generate_temporal_report <- function(temporal_result,
                                    stability_result = NULL,
                                    output_file = "temporal_analysis_report.html",
                                    format = "html",
                                    include_plots = TRUE) {
  
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package 'rmarkdown' is required for report generation")
  }
  
  # This would typically create an R Markdown document and render it
  # For now, we'll create a simple summary
  
  cat("Report generation is a planned feature.\n")
  cat("Use plot_temporal_segments() for visualizations and\n")
  cat("print() methods for summaries.\n")
  
  invisible(NULL)
}

#' Plot Temporal Stability Metrics
#'
#' Convenience function for visualizing temporal stability metrics. This is a wrapper
#' for \code{plot_temporal_segments(type = "stability")} that provides a more intuitive
#' function name for stability-specific visualizations.
#'
#' @param stability_result A temporal_stability object from \code{temporal_stability_analysis()}.
#' @param color_scheme Character string specifying color scheme: "default", "viridis",
#'   "RdYlBu", or "Set3". Default is "default".
#' @param show_labels Logical indicating whether to show segment labels. Default is TRUE.
#' @param title Character string for plot title. If NULL, auto-generated.
#' @param ... Additional arguments passed to \code{plot_temporal_segments()}.
#'
#' @return A ggplot2 object showing stability metrics over time, including:
#'   \itemize{
#'     \item Overall stability trends
#'     \item Number of active segments per window
#'     \item Change point indicators
#'     \item Volatility patterns
#'   }
#'
#' @details
#' This function creates multi-panel visualizations of stability metrics computed
#' by \code{temporal_stability_analysis()}. The plots help identify:
#' \itemize{
#'   \item Periods of structural stability vs. change
#'   \item Trends in segment formation and dissolution
#'   \item Critical transition points
#'   \item Overall system volatility
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
#' temporal_result <- moneca_temporal(months_data, window_size = 3)
#' 
#' # Compute stability metrics
#' stability <- temporal_stability_analysis(temporal_result)
#' 
#' # Plot stability metrics
#' plot_temporal_stability(stability)
#' 
#' # Customize visualization
#' plot_temporal_stability(stability, 
#'                        color_scheme = "viridis",
#'                        title = "Mobility Network Stability Over Time")
#' }
#'
#' @seealso 
#' \code{\link{temporal_stability_analysis}} for computing stability metrics,
#' \code{\link{plot_temporal_segments}} for other visualization types,
#' \code{\link{moneca_temporal}} for temporal clustering analysis
#'
#' @export
plot_temporal_stability <- function(stability_result,
                                  color_scheme = "default",
                                  show_labels = TRUE,
                                  title = NULL,
                                  ...) {
  
  # Validate input
  if (!inherits(stability_result, "temporal_stability")) {
    stop("Input must be a temporal_stability object from temporal_stability_analysis()")
  }
  
  # Call the main plotting function with type = "stability"
  plot_temporal_segments(
    temporal_result = stability_result,
    type = "stability",
    color_scheme = color_scheme,
    show_labels = show_labels,
    title = title,
    ...
  )
}