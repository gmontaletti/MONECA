#' Visualization Functions for Cut-off and RR Analysis
#'
#' Functions for visualizing the relationship between cut-off values,
#' relative risk distributions, and strength-based metrics.
#'
#' @name cutoff_visualization
#' @keywords internal
NULL

#' Plot Cut-off Sensitivity Analysis
#'
#' Creates a multi-panel plot showing how various metrics change with cut-off values.
#'
#' @param sensitivity_results Output from cutoff_sensitivity_analysis()
#' @param optimal_cutoff Optional optimal cut-off value to highlight
#' @param metrics Character vector of metrics to plot. Default includes key metrics
#' @param colors Color palette for the plots
#'
#' @return ggplot2 object with multiple panels
#'
#' @examples
#' \donttest{
#' # Generate synthetic mobility data  
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Run sensitivity analysis
#' sensitivity <- cutoff_sensitivity_analysis(data, n_points = 20, verbose = FALSE)
#' 
#' # Find optimal cut-off
#' optimal <- find_optimal_cutoff(data, criterion = "elbow", verbose = FALSE)
#' 
#' # Plot sensitivity analysis with optimal point
#' if (require(ggplot2, quietly = TRUE) && require(tidyr, quietly = TRUE)) {
#'   p <- plot_cutoff_analysis(sensitivity, optimal$optimal_cutoff)
#'   print(p)
#' }
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom tidyr pivot_longer
plot_cutoff_analysis <- function(sensitivity_results, 
                                optimal_cutoff = NULL,
                                metrics = c("strength_ratio", "total_strength", 
                                          "n_edges", "strength_cv"),
                                colors = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is required for this function")
  }
  
  # Validate inputs
  if (!inherits(sensitivity_results, "data.frame")) {
    stop("sensitivity_results must be a data.frame")
  }
  
  # Check if sensitivity_results is the wrong input (e.g., raw mobility data)
  if (!"cutoff" %in% colnames(sensitivity_results)) {
    stop("Input appears to be raw mobility data. Please run cutoff_sensitivity_analysis() first.")
  }
  
  # Filter metrics to only those that exist in the data
  available_metrics <- intersect(metrics, colnames(sensitivity_results))
  
  if (length(available_metrics) == 0) {
    stop("None of the requested metrics are available in the data. Available columns: ", 
         paste(colnames(sensitivity_results), collapse = ", "))
  }
  
  if (length(available_metrics) < length(metrics)) {
    missing_metrics <- setdiff(metrics, available_metrics)
    warning("Some requested metrics not found: ", paste(missing_metrics, collapse = ", "),
            ". Using available metrics: ", paste(available_metrics, collapse = ", "))
  }
  
  # Prepare data for plotting
  plot_data <- sensitivity_results[, c("cutoff", available_metrics), drop = FALSE]
  
  # Remove rows with non-finite values
  plot_data <- plot_data[complete.cases(plot_data), ]
  
  if (nrow(plot_data) == 0) {
    stop("No complete cases found in the data")
  }
  
  # Normalize metrics for comparison
  for (metric in available_metrics) {
    max_val <- max(plot_data[[metric]], na.rm = TRUE)
    if (is.finite(max_val) && max_val > 0) {
      plot_data[[paste0(metric, "_norm")]] <- plot_data[[metric]] / max_val
    } else {
      plot_data[[paste0(metric, "_norm")]] <- 0
    }
  }
  
  # Convert to long format
  long_data <- tidyr::pivot_longer(
    plot_data,
    cols = ends_with("_norm"),
    names_to = "metric",
    values_to = "value"
  )
  
  # Clean metric names for display
  long_data$metric_label <- gsub("_norm$", "", long_data$metric)
  long_data$metric_label <- gsub("_", " ", long_data$metric_label)
  long_data$metric_label <- tools::toTitleCase(long_data$metric_label)
  
  # Create the plot
  p <- ggplot(long_data, aes(x = cutoff, y = value, color = metric_label)) +
    geom_line(size = 1.2) +
    facet_wrap(~ metric_label, scales = "free_y", ncol = 2) +
    scale_color_manual(values = rep(colors, length.out = length(unique(long_data$metric_label)))) +
    labs(
      title = "Cut-off Sensitivity Analysis",
      subtitle = "How network metrics change with cut-off threshold",
      x = "Cut-off Value",
      y = "Normalized Metric Value"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12, color = "gray50"),
      strip.text = element_text(size = 10, face = "bold")
    )
  
  # Add optimal cut-off line if provided
  if (!is.null(optimal_cutoff)) {
    p <- p + geom_vline(xintercept = optimal_cutoff, 
                       linetype = "dashed", 
                       color = "red", 
                       alpha = 0.5) +
      annotate("text", x = optimal_cutoff, y = Inf, 
               label = "Optimal", 
               hjust = -0.1, vjust = 1.5, 
               color = "red", size = 3)
  }
  
  return(p)
}

#' Plot RR Distribution with Cut-off Thresholds
#'
#' Visualizes the distribution of relative risk values with various cut-off thresholds.
#'
#' @param mx Mobility matrix
#' @param cutoff_values Vector of cut-off values to highlight
#' @param small.cell.reduction Small cell adjustment parameter
#' @param plot_type Type of plot: "histogram", "density", or "both"
#'
#' @return ggplot2 object
#'
#' @examples
#' \donttest{
#' # Generate synthetic mobility data
#' data <- generate_mobility_data(n_classes = 4, seed = 123)
#' 
#' # Plot RR distribution with cut-off thresholds
#' if (require(ggplot2, quietly = TRUE)) {
#'   p <- plot_rr_distribution(data, cutoff_values = c(1.0, 1.5, 2.0))
#'   print(p)
#' }
#' }
#'
#' @export
plot_rr_distribution <- function(mx, 
                                cutoff_values = c(0.5, 1, 1.5, 2),
                                small.cell.reduction = 0,
                                plot_type = "both") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  # Get RR distribution
  rr_dist <- analyze_rr_distribution(mx, small.cell.reduction)
  rr_values <- weight.matrix(mx, cut.off = 0, small.cell.reduction = small.cell.reduction)
  
  # Filter for valid, positive, finite values only
  rr_values <- rr_values[!is.na(rr_values) & is.finite(rr_values) & rr_values > 0]
  
  if (length(rr_values) == 0) {
    stop("No valid RR values found in the matrix")
  }
  
  # Create data frame for plotting
  rr_df <- data.frame(rr = rr_values)
  
  # Base plot
  p <- ggplot(rr_df, aes(x = rr))
  
  # Add histogram and/or density
  if (plot_type %in% c("histogram", "both")) {
    p <- p + geom_histogram(aes(y = after_stat(density)), 
                           bins = 50, 
                           fill = "lightblue", 
                           alpha = 0.5,
                           color = "darkblue")
  }
  
  if (plot_type %in% c("density", "both")) {
    p <- p + geom_density(fill = "blue", alpha = 0.3, color = "darkblue", size = 1)
  }
  
  # Add cut-off lines
  cutoff_df <- data.frame(
    cutoff = cutoff_values,
    label = paste0("Cut-off = ", cutoff_values)
  )
  
  # Calculate retention percentages
  for (i in seq_along(cutoff_values)) {
    retention <- mean(rr_values >= cutoff_values[i]) * 100
    cutoff_df$retention[i] <- sprintf("%.1f%% retained", retention)
  }
  
  p <- p + geom_vline(data = cutoff_df, 
                     aes(xintercept = cutoff),
                     linetype = "dashed",
                     color = "red",
                     alpha = 0.6)
  
  # Add labels for cut-off lines
  max_density <- max(density(rr_values)$y)
  p <- p + geom_text(data = cutoff_df,
                    aes(x = cutoff, y = max_density * 0.9, label = retention),
                    angle = 90,
                    vjust = -0.5,
                    hjust = 1,
                    size = 3,
                    color = "red")
  
  # Add suggested cut-offs as points
  suggested_df <- data.frame(
    x = rr_dist$suggested_cutoffs,
    y = 0,
    label = names(rr_dist$suggested_cutoffs)
  )
  
  p <- p + geom_point(data = suggested_df,
                     aes(x = x, y = y),
                     color = "green",
                     size = 3,
                     shape = 17)
  
  # Formatting
  p <- p + 
    labs(
      title = "Relative Risk Distribution",
      subtitle = sprintf("N = %d values, Range: %.2f to %.2f", 
                        length(rr_values), min(rr_values), max(rr_values)),
      x = "Relative Risk Value",
      y = "Density",
      caption = "Red lines: cut-off thresholds | Green triangles: suggested cut-offs"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray50"),
      plot.caption = element_text(size = 9, color = "gray60")
    ) +
    scale_x_continuous(limits = c(0, min(max(rr_values), 5)))
  
  return(p)
}

#' Plot Strength Comparison Across Cut-offs
#'
#' Creates box plots comparing strength distributions at different cut-off values.
#'
#' @param mx Mobility matrix
#' @param cutoff_values Vector of cut-off values to compare
#' @param small.cell.reduction Small cell adjustment
#'
#' @return ggplot2 object
#'
#' @export
plot_strength_comparison <- function(mx, 
                                    cutoff_values = c(0.5, 1, 1.5, 2),
                                    small.cell.reduction = 0) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  # Collect strength data for each cut-off
  strength_data <- list()
  
  for (co in cutoff_values) {
    weight_mat <- weight.matrix(mx, cut.off = co, 
                               small.cell.reduction = small.cell.reduction)
    weight_mat[is.na(weight_mat)] <- 0
    
    g <- moneca_graph_from_adjacency(weight_mat, mode = "undirected", 
                                     weighted = TRUE, diag = FALSE)
    
    strengths <- igraph::strength(g, mode = "all")
    
    # Filter for positive, finite strengths
    valid_strengths <- strengths[is.finite(strengths) & strengths > 0]
    
    if (length(valid_strengths) > 0) {
      strength_data[[as.character(co)]] <- data.frame(
        strength = valid_strengths,
        cutoff = factor(co, levels = cutoff_values)
      )
    }
  }
  
  # Remove empty entries
  strength_data <- strength_data[lengths(strength_data) > 0]
  
  # Combine all data
  all_strength <- do.call(rbind, strength_data)
  
  # Create box plot
  p <- ggplot(all_strength, aes(x = cutoff, y = strength, fill = cutoff)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.size = 1) +
    geom_violin(alpha = 0.3) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Strength Distribution Across Cut-off Values",
      subtitle = "How node strength changes with different cut-off thresholds",
      x = "Cut-off Value",
      y = "Node Strength",
      fill = "Cut-off"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray50"),
      legend.position = "none"
    )
  
  # Add summary statistics as text
  summary_stats <- aggregate(strength ~ cutoff, all_strength, 
                            function(x) c(mean = mean(x), median = median(x)))
  
  # Add mean lines
  p <- p + geom_hline(data = data.frame(
                       yintercept = summary_stats$strength[, "mean"],
                       cutoff = summary_stats$cutoff),
                     aes(yintercept = yintercept, color = cutoff),
                     linetype = "dashed",
                     alpha = 0.5) +
    scale_color_brewer(palette = "Set2", guide = "none")
  
  return(p)
}

#' Create Interactive Cut-off Explorer
#'
#' Creates an interactive plot for exploring cut-off effects using plotly.
#'
#' @param sensitivity_results Output from cutoff_sensitivity_analysis()
#' @param metrics Metrics to include in the interactive plot
#'
#' @return plotly object
#'
#' @export
plot_cutoff_interactive <- function(sensitivity_results,
                                   metrics = c("strength_ratio", "n_edges", 
                                             "modularity", "strength_cv")) {
  
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package is required for this function")
  }
  
  # Create subplot for each metric
  plots <- list()
  
  for (i in seq_along(metrics)) {
    metric <- metrics[i]
    
    p <- plotly::plot_ly(
      data = sensitivity_results,
      x = ~cutoff,
      y = ~get(metric),
      type = 'scatter',
      mode = 'lines',
      name = metric,
      line = list(width = 2)
    ) %>%
      plotly::layout(
        xaxis = list(title = if(i == length(metrics)) "Cut-off" else ""),
        yaxis = list(title = gsub("_", " ", metric))
      )
    
    plots[[i]] <- p
  }
  
  # Combine subplots
  fig <- plotly::subplot(plots, nrows = length(metrics), shareX = TRUE) %>%
    plotly::layout(
      title = "Interactive Cut-off Sensitivity Explorer",
      showlegend = FALSE,
      height = 200 * length(metrics)
    )
  
  return(fig)
}

#' Plot Elbow Curve for Optimal Cut-off
#'
#' Visualizes the elbow curve used to identify optimal cut-off.
#'
#' @param sensitivity_results Sensitivity analysis results
#' @param optimal_cutoff Optimal cut-off value found
#' @param metric Metric to use for elbow detection
#'
#' @return ggplot2 object
#'
#' @export
plot_elbow_curve <- function(sensitivity_results,
                            optimal_cutoff = NULL,
                            metric = "strength_ratio") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for this function")
  }
  
  # Extract the metric
  x <- sensitivity_results$cutoff
  y <- sensitivity_results[[metric]]
  
  # Create base plot
  p <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
    geom_line(size = 1.2, color = "darkblue") +
    geom_point(size = 2, color = "darkblue") +
    labs(
      title = paste("Elbow Curve for", gsub("_", " ", metric)),
      subtitle = "Identifying the optimal cut-off point",
      x = "Cut-off Value",
      y = tools::toTitleCase(gsub("_", " ", metric))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11, color = "gray50")
    )
  
  # Add optimal point if provided
  if (!is.null(optimal_cutoff)) {
    optimal_idx <- which.min(abs(x - optimal_cutoff))
    optimal_y <- y[optimal_idx]
    
    p <- p + 
      geom_point(aes(x = optimal_cutoff, y = optimal_y),
                color = "red", size = 4) +
      geom_vline(xintercept = optimal_cutoff,
                linetype = "dashed", color = "red", alpha = 0.5) +
      annotate("text", x = optimal_cutoff, y = optimal_y,
              label = sprintf("Optimal\n(%.2f)", optimal_cutoff),
              hjust = -0.2, vjust = 0,
              color = "red", size = 3, fontface = "bold")
  }
  
  # Add shaded region for recommended range
  if (!is.null(optimal_cutoff)) {
    range_width <- diff(range(x)) * 0.1
    p <- p + 
      annotate("rect",
              xmin = optimal_cutoff - range_width/2,
              xmax = optimal_cutoff + range_width/2,
              ymin = -Inf, ymax = Inf,
              fill = "red", alpha = 0.1)
  }
  
  return(p)
}