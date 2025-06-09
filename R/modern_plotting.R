#' Modern MONECA Plotting with ggraph
#'
#' Enhanced plotting functions using ggplot2 and ggraph for MONECA objects.
#' These functions replace the old plotting system with modern, customizable
#' network visualizations.
#'
#' @name modern_plotting
NULL

#' Plot MONECA results with ggraph
#'
#' Creates modern network visualizations of MONECA clustering results using
#' ggraph and ggplot2. This function replaces the old gg.moneca function
#' with improved performance and customization options.
#'
#' @param segments A MONECA object from \code{\link{moneca}}
#' @param level Integer vector. Which segmentation levels to display (default: all except first)
#' @param layout Layout algorithm or matrix. Can be "fr", "kk", "dh", "mds", or a coordinate matrix
#' @param edges Edge matrix or "auto" to use segment.edges()
#' @param node_size Aesthetic for node size. Can be "total", "mobility", or numeric vector
#' @param node_color Aesthetic for node color. Can be "segment", "mobility", or color vector
#' @param node_alpha Numeric. Node transparency (0-1, default: 0.8)
#' @param edge_width Aesthetic for edge width. Can be "weight" or numeric
#' @param edge_color Color for edges (default: "grey50")
#' @param edge_alpha Numeric. Edge transparency (0-1, default: 0.6)
#' @param show_labels Logical. Whether to show node labels (default: TRUE)
#' @param label_size Numeric. Size of node labels (default: 3)
#' @param show_segments Logical. Whether to show segment boundaries (default: TRUE)
#' @param segment_alpha Numeric. Segment boundary transparency (0-1, default: 0.3)
#' @param color_palette Character. Color palette name for segments (default: "Set3")
#' @param theme_style Character. Plot theme: "minimal", "void", or "classic" (default: "void")
#' @param title Character. Plot title (optional)
#' @param ... Additional arguments passed to ggraph
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' # Basic plot
#' data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(data, segment.levels = 3)
#' plot_moneca_ggraph(seg)
#'
#' # Customized plot
#' plot_moneca_ggraph(seg,
#'   layout = "kk",
#'   node_color = "segment",
#'   edge_width = "weight",
#'   color_palette = "Spectral",
#'   title = "Social Mobility Network"
#' )
#' }
#'
#' @export
plot_moneca_ggraph <- function(segments,
                              level = seq(segments$segment.list)[-1],
                              layout = "fr",
                              edges = "auto",
                              node_size = "total",
                              node_color = "segment",
                              node_alpha = 0.8,
                              edge_width = "weight",
                              edge_color = "grey50",
                              edge_alpha = 0.6,
                              show_labels = TRUE,
                              label_size = 3,
                              show_segments = TRUE,
                              segment_alpha = 0.3,
                              color_palette = "Set3",
                              theme_style = "void",
                              title = NULL,
                              ...) {
  
  # Load required packages
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required for this function. Please install it.")
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' is required for this function. Please install it.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function. Please install it.")
  }
  
  # Get edge matrix
  if (identical(edges, "auto")) {
    edge_matrix <- segment.edges(segments, level = 1)
  } else {
    edge_matrix <- edges
  }
  
  # Create igraph object
  g <- moneca_graph_from_adjacency(edge_matrix, mode = "directed", weighted = TRUE)
  
  # Get node information
  n_nodes <- length(V(g))
  node_names <- V(g)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node_", 1:n_nodes)
    V(g)$name <- node_names
  }
  
  # Convert to tidygraph
  tg <- tidygraph::as_tbl_graph(g)
  
  # Add node attributes
  mobility_matrix <- segments$mat.list[[1]]
  
  # Calculate node sizes
  if (identical(node_size, "total")) {
    node_totals <- (mobility_matrix[nrow(mobility_matrix), ] + 
                   mobility_matrix[, nrow(mobility_matrix)]) / 2
    node_totals <- node_totals[-length(node_totals)]
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = node_totals)
  } else if (identical(node_size, "mobility")) {
    # Calculate mobility rates (off-diagonal mobility)
    mobility_rates <- 1 - diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) / 
                     rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = mobility_rates)
  } else if (is.numeric(node_size) && length(node_size) == n_nodes) {
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = node_size)
  } else {
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = 5)
  }
  
  # Add segment membership
  if (identical(node_color, "segment")) {
    membership <- segment.membership(segments, level = level)
    # Match node names to membership
    node_segments <- membership$membership[match(node_names, membership$name)]
    node_segments[is.na(node_segments)] <- "Unassigned"
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(segment = as.factor(node_segments))
  } else if (identical(node_color, "mobility")) {
    mobility_rates <- 1 - diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) / 
                     rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tg <- tg %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(mobility_rate = mobility_rates)
  }
  
  # Create base plot
  if (is.character(layout)) {
    p <- ggraph::ggraph(tg, layout = layout, ...)
  } else {
    p <- ggraph::ggraph(tg, layout = "manual", x = layout[, 1], y = layout[, 2])
  }
  
  # Add edges
  if (identical(edge_width, "weight")) {
    p <- p + ggraph::geom_edge_link(
      ggplot2::aes(width = weight),
      color = edge_color,
      alpha = edge_alpha,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
      end_cap = ggraph::circle(0.1, "cm")
    ) +
    ggplot2::scale_edge_width_continuous(range = c(0.2, 2), guide = "none")
  } else {
    edge_width_val <- if (is.numeric(edge_width)) edge_width else 0.5
    p <- p + ggraph::geom_edge_link(
      color = edge_color,
      alpha = edge_alpha,
      width = edge_width_val,
      arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
      end_cap = ggraph::circle(0.1, "cm")
    )
  }
  
  # Add nodes
  if (identical(node_color, "segment")) {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size, color = segment),
      alpha = node_alpha
    ) +
    ggplot2::scale_color_brewer(type = "qual", palette = color_palette, name = "Segment")
  } else if (identical(node_color, "mobility")) {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size, color = mobility_rate),
      alpha = node_alpha
    ) +
    ggplot2::scale_color_gradient(low = "blue", high = "red", name = "Mobility Rate")
  } else {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size),
      color = node_color,
      alpha = node_alpha
    )
  }
  
  # Add labels
  if (show_labels) {
    p <- p + ggraph::geom_node_text(
      ggplot2::aes(label = name),
      size = label_size,
      repel = TRUE
    )
  }
  
  # Add segment boundaries (convex hulls)
  if (show_segments && identical(node_color, "segment")) {
    # This would require additional computation to create convex hulls
    # For now, we'll skip this feature but it can be added later
  }
  
  # Apply theme
  if (theme_style == "void") {
    p <- p + ggraph::theme_graph()
  } else if (theme_style == "minimal") {
    p <- p + ggplot2::theme_minimal() +
           ggplot2::theme(
             axis.text = ggplot2::element_blank(),
             axis.title = ggplot2::element_blank(),
             panel.grid = ggplot2::element_blank()
           )
  } else if (theme_style == "classic") {
    p <- p + ggplot2::theme_classic() +
           ggplot2::theme(
             axis.text = ggplot2::element_blank(),
             axis.title = ggplot2::element_blank()
           )
  }
  
  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  
  # Scale node sizes
  p <- p + ggplot2::scale_size_continuous(range = c(2, 8), name = "Size")
  
  return(p)
}

#' Create ego network plot with ggraph
#'
#' Visualizes the mobility patterns from a single node (occupation/class)
#' using modern ggraph plotting.
#'
#' @param segments A MONECA object
#' @param mobility_matrix The original mobility matrix
#' @param ego_id Integer or character. The ID or name of the focal node
#' @param layout Layout for the network (default: "stress")
#' @param highlight_color Color for the ego node (default: "red")
#' @param flow_color Color scheme for mobility flows (default: "viridis")
#' @param node_size_range Numeric vector of length 2. Range for node sizes
#' @param edge_width_range Numeric vector of length 2. Range for edge widths
#' @param title Character. Plot title (optional)
#' @param ... Additional arguments
#'
#' @return A ggplot object
#'
#' @examples
#' \dontrun{
#' data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(data, segment.levels = 3)
#' plot_ego_ggraph(seg, data, ego_id = 3, title = "Mobility from Class 3")
#' }
#'
#' @export
plot_ego_ggraph <- function(segments,
                           mobility_matrix,
                           ego_id,
                           layout = "stress",
                           highlight_color = "red",
                           flow_color = "viridis",
                           node_size_range = c(2, 8),
                           edge_width_range = c(0.2, 3),
                           title = NULL,
                           ...) {
  
  # Load required packages
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required for this function. Please install it.")
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop("Package 'tidygraph' is required for this function. Please install it.")
  }
  
  # Extract the core mobility matrix (without totals)
  n_classes <- nrow(mobility_matrix) - 1
  core_matrix <- mobility_matrix[1:n_classes, 1:n_classes]
  
  # Convert ego_id to numeric if character
  if (is.character(ego_id)) {
    ego_id <- which(rownames(core_matrix) == ego_id)
    if (length(ego_id) == 0) {
      stop("ego_id not found in matrix row names")
    }
  }
  
  # Create ego network matrix (only flows from/to ego)
  ego_matrix <- matrix(0, nrow = n_classes, ncol = n_classes)
  ego_matrix[ego_id, ] <- core_matrix[ego_id, ]  # Outflows
  ego_matrix[, ego_id] <- core_matrix[, ego_id]  # Inflows
  
  # Remove zero-flow edges for cleaner visualization
  ego_matrix[ego_matrix == 0] <- NA
  
  # Create graph
  g <- moneca_graph_from_adjacency(ego_matrix, mode = "directed", weighted = TRUE)
  tg <- tidygraph::as_tbl_graph(g)
  
  # Add node attributes
  node_names <- V(g)$name
  if (is.null(node_names)) {
    node_names <- rownames(core_matrix)
    V(g)$name <- node_names
  }
  
  # Node sizes based on total mobility
  node_totals <- rowSums(core_matrix) + colSums(core_matrix)
  tg <- tg %>% tidygraph::activate(nodes) %>%
        dplyr::mutate(
          node_size = node_totals,
          is_ego = 1:tidygraph::graph_order() == ego_id,
          node_name = node_names
        )
  
  # Create plot
  p <- ggraph::ggraph(tg, layout = layout, ...)
  
  # Add edges with width based on flow volume
  p <- p + ggraph::geom_edge_link(
    ggplot2::aes(width = weight, color = weight),
    alpha = 0.7,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"), type = "closed"),
    end_cap = ggraph::circle(0.15, "cm")
  ) +
  ggplot2::scale_edge_width_continuous(range = edge_width_range, guide = "none") +
  ggplot2::scale_edge_color_viridis_c(name = "Flow Volume")
  
  # Add nodes
  p <- p + ggraph::geom_node_point(
    ggplot2::aes(size = node_size, color = is_ego),
    alpha = 0.8
  ) +
  ggplot2::scale_color_manual(
    values = c("FALSE" = "steelblue", "TRUE" = highlight_color),
    guide = "none"
  ) +
  ggplot2::scale_size_continuous(range = node_size_range, name = "Total Mobility")
  
  # Add labels
  p <- p + ggraph::geom_node_text(
    ggplot2::aes(label = node_name),
    size = 3,
    repel = TRUE
  )
  
  # Apply theme
  p <- p + ggraph::theme_graph()
  
  # Add title
  if (is.null(title)) {
    ego_name <- node_names[ego_id]
    title <- paste("Mobility Network for", ego_name)
  }
  p <- p + ggplot2::ggtitle(title)
  
  return(p)
}

#' Create stair plot showing segmentation levels
#'
#' Creates a series of plots showing how segmentation changes across levels
#' using modern ggraph visualization.
#'
#' @param segments A MONECA object
#' @param levels Integer vector. Which levels to show (default: all)
#' @param layout Layout to use across all plots for consistency
#' @param ncol Integer. Number of columns in the plot grid (default: 2)
#' @param ... Additional arguments passed to plot_moneca_ggraph
#'
#' @return A list of ggplot objects or a combined plot grid
#'
#' @examples
#' \dontrun{
#' data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(data, segment.levels = 4)
#' plots <- plot_stair_ggraph(seg)
#' }
#'
#' @export
plot_stair_ggraph <- function(segments,
                             levels = seq_along(segments$segment.list)[-1],
                             layout = NULL,
                             ncol = 2,
                             ...) {
  
  # Create consistent layout if not provided
  if (is.null(layout)) {
    # Use the layout.matrix function from MONECA
    layout <- layout.matrix(segments)
  }
  
  plots <- list()
  
  for (i in seq_along(levels)) {
    level_idx <- levels[i]
    
    p <- plot_moneca_ggraph(
      segments,
      level = 1:level_idx,
      layout = layout,
      title = paste("Level", level_idx, "Segmentation"),
      ...
    )
    
    plots[[i]] <- p
  }
  
  names(plots) <- paste("Level", levels)
  
  return(plots)
}