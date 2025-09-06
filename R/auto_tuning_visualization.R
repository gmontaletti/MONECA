# ============================================================================
# VISUALIZATION FUNCTIONS FOR AUTO-TUNING RESULTS
# ============================================================================

#' Plot Tuning Results with Enhanced Visualizations
#'
#' Creates comprehensive visualization of auto-tuning results with multiple plot types
#' and advanced aesthetics.
#'
#' @param tuning_result Result object from auto_tune_small_cell_reduction.
#' @param plot_type Type of plot: "overview", "quality_vs_performance", 
#'   "pareto", "sensitivity", "convergence", or "all".
#' @param save_plot Whether to save plot to file.
#' @param filename Filename for saved plot.
#' @param width Plot width in inches.
#' @param height Plot height in inches.
#' @return ggplot object or list of ggplot objects.
#' @export
plot_tuning_results <- function(tuning_result, 
                              plot_type = "overview",
                              save_plot = FALSE,
                              filename = "moneca_tuning_results.pdf",
                              width = 12,
                              height = 8) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for plotting")
  }
  
  # Extract data for plotting
  candidates <- tuning_result$candidates_tested
  quality_scores <- sapply(tuning_result$quality_metrics, function(x) x$overall)
  perf_times <- sapply(tuning_result$performance_metrics, function(x) x$time)
  stability_scores <- tuning_result$stability_scores
  
  plots <- list()
  
  if (plot_type %in% c("overview", "all")) {
    plots$overview <- create_overview_plot(tuning_result, candidates, quality_scores, 
                                          perf_times, stability_scores)
  }
  
  if (plot_type %in% c("quality_vs_performance", "all")) {
    plots$quality_vs_performance <- create_quality_performance_plot(tuning_result, 
                                                                   candidates, quality_scores, perf_times)
  }
  
  if (plot_type %in% c("pareto", "all") && !is.null(tuning_result$pareto_solutions)) {
    plots$pareto <- create_pareto_plot(tuning_result)
  }
  
  if (plot_type %in% c("sensitivity", "all")) {
    plots$sensitivity <- create_sensitivity_plot(tuning_result, candidates, 
                                                quality_scores, stability_scores)
  }
  
  if (plot_type %in% c("convergence", "all") && !is.null(tuning_result$bayesian_results)) {
    plots$convergence <- create_convergence_plot(tuning_result)
  }
  
  # Return appropriate plot(s)
  result_plot <- handle_plot_output(plots, plot_type, save_plot, filename, width, height)
  
  return(result_plot)
}

#' Create Overview Plot
#'
#' Creates main overview plot showing quality, performance, and stability.
#'
#' @param tuning_result Tuning results object.
#' @param candidates Vector of candidates.
#' @param quality_scores Vector of quality scores.
#' @param perf_times Vector of performance times.
#' @param stability_scores Vector of stability scores.
#' @return ggplot object.
#' @keywords internal
create_overview_plot <- function(tuning_result, candidates, quality_scores, 
                                perf_times, stability_scores) {
  
  # Prepare data for plotting
  valid_perf <- !is.na(perf_times) & perf_times > 0
  performance_scores <- rep(0, length(perf_times))
  performance_scores[valid_perf] <- 1 / perf_times[valid_perf]
  
  # Normalize scores to [0, 1] for comparison
  normalize_score <- function(x) {
    if (max(x, na.rm = TRUE) > min(x, na.rm = TRUE)) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    } else {
      rep(0.5, length(x))
    }
  }
  
  plot_data <- data.frame(
    candidate = rep(candidates, 3),
    value = c(normalize_score(quality_scores), 
             normalize_score(performance_scores),
             normalize_score(stability_scores)),
    metric = rep(c("Quality", "Performance", "Stability"), each = length(candidates)),
    raw_value = c(quality_scores, performance_scores, stability_scores)
  )
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = candidate, y = value, color = metric)) +
    ggplot2::geom_line(size = 1.2, alpha = 0.8) +
    ggplot2::geom_point(size = 2.5, alpha = 0.9) +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.3, size = 0.5) +
    ggplot2::geom_vline(xintercept = tuning_result$optimal_value, 
                       linetype = "dashed", color = "red", size = 1) +
    ggplot2::scale_color_manual(values = c("Quality" = "#2E86AB", 
                                          "Performance" = "#A23B72", 
                                          "Stability" = "#F18F01")) +
    ggplot2::labs(
      title = paste("MONECA Auto-Tuning Results:", tuning_result$tuning_method),
      x = "small.cell.reduction parameter",
      y = "Normalized Score (0-1)",
      color = "Metric",
      subtitle = paste("Optimal value:", tuning_result$optimal_value, 
                      "| Computation time:", round(tuning_result$computation_time, 2), "seconds")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
    )
}

#' Create Quality vs Performance Plot
#'
#' Creates scatter plot showing quality-performance trade-offs.
#'
#' @param tuning_result Tuning results object.
#' @param candidates Vector of candidates.
#' @param quality_scores Vector of quality scores.
#' @param perf_times Vector of performance times.
#' @return ggplot object.
#' @keywords internal
create_quality_performance_plot <- function(tuning_result, candidates, 
                                           quality_scores, perf_times) {
  
  # Filter valid data
  valid_idx <- !is.na(perf_times) & perf_times > 0 & !is.na(quality_scores)
  
  if (sum(valid_idx) == 0) {
    # Return empty plot with message
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0, y = 0, label = "No valid performance data available") +
           ggplot2::theme_void())
  }
  
  plot_data <- data.frame(
    candidate = candidates[valid_idx],
    quality = quality_scores[valid_idx],
    time = perf_times[valid_idx],
    performance = 1 / perf_times[valid_idx],
    efficiency = quality_scores[valid_idx] / perf_times[valid_idx]
  )
  
  # Find optimal point
  optimal_point <- plot_data[plot_data$candidate == tuning_result$optimal_value, ]
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = performance, y = quality)) +
    ggplot2::geom_point(ggplot2::aes(color = candidate, size = efficiency), alpha = 0.7) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, color = "gray50", alpha = 0.5) +
    ggplot2::scale_color_gradient2(low = "blue", mid = "purple", high = "red",
                                  midpoint = median(plot_data$candidate)) +
    ggplot2::scale_size_continuous(range = c(2, 6), guide = "legend") +
    ggplot2::labs(
      title = "Quality vs Performance Trade-off Analysis",
      x = "Performance (1/time)",
      y = "Quality Score",
      color = "Parameter\nValue",
      size = "Efficiency\n(Quality/Time)",
      subtitle = "Color represents parameter value, size represents efficiency"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Highlight optimal point if available
  if (nrow(optimal_point) > 0) {
    p <- p + ggplot2::geom_point(data = optimal_point, 
                                ggplot2::aes(x = performance, y = quality),
                                color = "black", size = 8, shape = 21, 
                                fill = "yellow", stroke = 2)
  }
  
  return(p)
}

#' Create Pareto Frontier Plot
#'
#' Creates plot showing Pareto frontier analysis.
#'
#' @param tuning_result Tuning results object.
#' @return ggplot object.
#' @keywords internal
create_pareto_plot <- function(tuning_result) {
  
  pareto_data <- as.data.frame(tuning_result$pareto_solutions$objective_matrix)
  pareto_data$candidate <- tuning_result$candidates_tested
  pareto_data$is_pareto <- seq_along(tuning_result$candidates_tested) %in% 
                          tuning_result$pareto_solutions$frontier_indices
  
  objectives <- tuning_result$pareto_solutions$objectives
  
  if (length(objectives) < 2) {
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0, y = 0, 
                            label = "Pareto analysis requires at least 2 objectives") +
           ggplot2::theme_void())
  }
  
  obj_x <- objectives[1]
  obj_y <- objectives[2]
  
  p <- ggplot2::ggplot(pareto_data, ggplot2::aes_string(x = obj_x, y = obj_y)) +
    ggplot2::geom_point(ggplot2::aes(color = is_pareto, size = is_pareto, alpha = is_pareto)) +
    ggplot2::scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#E31A1C"),
                               labels = c("FALSE" = "Dominated", "TRUE" = "Pareto Frontier")) +
    ggplot2::scale_size_manual(values = c("FALSE" = 2, "TRUE" = 5),
                              labels = c("FALSE" = "Dominated", "TRUE" = "Pareto Frontier")) +
    ggplot2::scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 1.0),
                               labels = c("FALSE" = "Dominated", "TRUE" = "Pareto Frontier")) +
    ggplot2::geom_text(data = subset(pareto_data, is_pareto),
                      ggplot2::aes(label = round(candidate, 2)), 
                      hjust = -0.1, vjust = -0.1, size = 3) +
    ggplot2::labs(
      title = "Pareto Frontier Analysis",
      x = tools::toTitleCase(gsub("_", " ", obj_x)),
      y = tools::toTitleCase(gsub("_", " ", obj_y)),
      color = "Solution Type",
      size = "Solution Type",
      alpha = "Solution Type",
      subtitle = paste("Red points represent non-dominated solutions on the Pareto frontier")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}

#' Create Sensitivity Analysis Plot
#'
#' Creates plot showing parameter sensitivity.
#'
#' @param tuning_result Tuning results object.
#' @param candidates Vector of candidates.
#' @param quality_scores Vector of quality scores.
#' @param stability_scores Vector of stability scores.
#' @return ggplot object.
#' @keywords internal
create_sensitivity_plot <- function(tuning_result, candidates, quality_scores, stability_scores) {
  
  n <- length(candidates)
  quality_sensitivity <- numeric(n)
  stability_sensitivity <- numeric(n)
  
  # Calculate sensitivity (local derivatives)
  for (i in seq_len(n)) {
    if (i > 1 && i < n) {
      dx_left <- candidates[i] - candidates[i-1]
      dx_right <- candidates[i+1] - candidates[i]
      
      if (dx_left > 0) {
        quality_sensitivity[i] <- abs((quality_scores[i+1] - quality_scores[i-1]) / 
                                     (candidates[i+1] - candidates[i-1]))
        stability_sensitivity[i] <- abs((stability_scores[i+1] - stability_scores[i-1]) / 
                                       (candidates[i+1] - candidates[i-1]))
      }
    } else if (i == 1 && n > 1) {
      dx <- candidates[i+1] - candidates[i]
      if (dx > 0) {
        quality_sensitivity[i] <- abs((quality_scores[i+1] - quality_scores[i]) / dx)
        stability_sensitivity[i] <- abs((stability_scores[i+1] - stability_scores[i]) / dx)
      }
    } else if (i == n && n > 1) {
      dx <- candidates[i] - candidates[i-1]
      if (dx > 0) {
        quality_sensitivity[i] <- abs((quality_scores[i] - quality_scores[i-1]) / dx)
        stability_sensitivity[i] <- abs((stability_scores[i] - stability_scores[i-1]) / dx)
      }
    }
  }
  
  plot_data <- data.frame(
    candidate = rep(candidates, 2),
    sensitivity = c(quality_sensitivity, stability_sensitivity),
    metric = rep(c("Quality", "Stability"), each = n)
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = candidate, y = sensitivity, color = metric)) +
    ggplot2::geom_line(size = 1.2, alpha = 0.8) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.3, size = 0.5) +
    ggplot2::geom_vline(xintercept = tuning_result$optimal_value, 
                       linetype = "dashed", color = "red", size = 1) +
    ggplot2::scale_color_manual(values = c("Quality" = "#1f77b4", "Stability" = "#ff7f0e")) +
    ggplot2::labs(
      title = "Parameter Sensitivity Analysis",
      x = "small.cell.reduction parameter",
      y = "Sensitivity (|derivative|)",
      color = "Metric",
      subtitle = paste("Dashed line shows optimal value:", tuning_result$optimal_value)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}

#' Create Convergence Plot for Bayesian Optimization
#'
#' Shows convergence behavior of Bayesian optimization.
#'
#' @param tuning_result Tuning results object.
#' @return ggplot object.
#' @keywords internal
create_convergence_plot <- function(tuning_result) {
  
  if (is.null(tuning_result$bayesian_results)) {
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0, y = 0, 
                            label = "No Bayesian optimization results available") +
           ggplot2::theme_void())
  }
  
  # Extract Bayesian optimization data
  bayesian_scores <- tuning_result$bayesian_results$bayesian_scores
  n_iterations <- length(bayesian_scores)
  
  # Calculate cumulative best
  cumulative_best <- numeric(n_iterations)
  cumulative_best[1] <- bayesian_scores[1]
  for (i in 2:n_iterations) {
    cumulative_best[i] <- max(cumulative_best[i-1], bayesian_scores[i])
  }
  
  plot_data <- data.frame(
    iteration = rep(seq_len(n_iterations), 2),
    score = c(bayesian_scores, cumulative_best),
    type = rep(c("Current", "Best So Far"), each = n_iterations)
  )
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = iteration, y = score, color = type)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::scale_color_manual(values = c("Current" = "#1f77b4", "Best So Far" = "#d62728")) +
    ggplot2::labs(
      title = "Bayesian Optimization Convergence",
      x = "Iteration",
      y = "Objective Score",
      color = "Score Type",
      subtitle = paste("Acquisition function:", tuning_result$bayesian_results$acquisition_function)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  return(p)
}

#' Handle Plot Output
#'
#' Handles the output and saving of plots.
#'
#' @param plots List of plots.
#' @param plot_type Type of plot requested.
#' @param save_plot Whether to save plot.
#' @param filename Filename for saving.
#' @param width Plot width.
#' @param height Plot height.
#' @return Plot object or list of plots.
#' @keywords internal
handle_plot_output <- function(plots, plot_type, save_plot, filename, width, height) {
  
  # Return appropriate plot(s)
  if (length(plots) == 1) {
    result_plot <- plots[[1]]
  } else if (plot_type == "all") {
    # Combine plots if patchwork is available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      result_plot <- patchwork::wrap_plots(plots, ncol = 2)
    } else {
      result_plot <- plots
      warning("patchwork package not available for combining plots. Returning list of plots.")
    }
  } else if (plot_type %in% names(plots)) {
    result_plot <- plots[[plot_type]]
  } else {
    stop("Invalid plot_type or no plots generated")
  }
  
  # Save plot if requested
  if (save_plot && !is.null(result_plot)) {
    if (is.list(result_plot) && !inherits(result_plot, "patchwork")) {
      # Save multiple plots to separate files
      for (i in seq_along(result_plot)) {
        plot_name <- names(result_plot)[i] %||% paste0("plot_", i)
        file_ext <- tools::file_ext(filename)
        file_base <- tools::file_path_sans_ext(filename)
        plot_filename <- paste0(file_base, "_", plot_name, ".", file_ext)
        
        ggplot2::ggsave(plot_filename, result_plot[[i]], width = width, height = height)
        cat("Plot saved to:", plot_filename, "\n")
      }
    } else {
      ggplot2::ggsave(filename, result_plot, width = width, height = height)
      cat("Plot saved to:", filename, "\n")
    }
  }
  
  return(result_plot)
}

#' Parameter Sensitivity Analysis Plot
#'
#' Creates detailed visualization showing parameter sensitivity across the range
#' with confidence intervals if available.
#'
#' @param tuning_result Result object from auto_tune_small_cell_reduction.
#' @param save_plot Whether to save plot to file.
#' @param filename Filename for saved plot.
#' @param include_ci Whether to include confidence intervals (if available).
#' @return ggplot object.
#' @export
parameter_sensitivity_plot <- function(tuning_result,
                                     save_plot = FALSE,
                                     filename = "parameter_sensitivity.pdf",
                                     include_ci = TRUE) {
  
  p <- create_sensitivity_plot(tuning_result, 
                              tuning_result$candidates_tested,
                              sapply(tuning_result$quality_metrics, function(x) x$overall),
                              tuning_result$stability_scores)
  
  # Add confidence intervals if available and requested
  if (include_ci && !is.null(tuning_result$cv_confidence_intervals)) {
    ci_data <- data.frame(
      candidate = tuning_result$candidates_tested,
      lower = tuning_result$cv_confidence_intervals[, "lower"],
      upper = tuning_result$cv_confidence_intervals[, "upper"]
    )
    
    p <- p + ggplot2::geom_ribbon(data = ci_data,
                                 ggplot2::aes(x = candidate, ymin = lower, ymax = upper),
                                 alpha = 0.2, fill = "gray50")
  }
  
  if (save_plot) {
    ggplot2::ggsave(filename, p, width = 10, height = 6)
    cat("Sensitivity plot saved to:", filename, "\n")
  }
  
  return(p)
}

#' Performance vs Quality Trade-off Plot
#'
#' Creates detailed visualization of performance-quality trade-offs with
#' efficiency contours and optimal regions.
#'
#' @param tuning_result Result object from auto_tune_small_cell_reduction.
#' @param save_plot Whether to save plot to file.
#' @param filename Filename for saved plot.
#' @param show_efficiency_contours Whether to show efficiency contour lines.
#' @return ggplot object.
#' @export
performance_quality_tradeoff_plot <- function(tuning_result,
                                            save_plot = FALSE,
                                            filename = "performance_quality_tradeoff.pdf",
                                            show_efficiency_contours = TRUE) {
  
  p <- create_quality_performance_plot(tuning_result,
                                      tuning_result$candidates_tested,
                                      sapply(tuning_result$quality_metrics, function(x) x$overall),
                                      sapply(tuning_result$performance_metrics, function(x) x$time))
  
  # Add efficiency contours if requested
  if (show_efficiency_contours) {
    # This would require additional computation
    # For now, just enhance the existing plot
    p <- p + ggplot2::labs(
      caption = "Larger points indicate higher efficiency (quality per unit time)"
    )
  }
  
  if (save_plot) {
    ggplot2::ggsave(filename, p, width = 12, height = 8)
    cat("Trade-off plot saved to:", filename, "\n")
  }
  
  return(p)
}