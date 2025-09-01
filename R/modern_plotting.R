#' Modern MONECA Plotting with ggraph
#'
#' Enhanced plotting functions using ggplot2 and ggraph for moneca objects.
#' These functions replace the old plotting system with modern, customizable
#' network visualizations.
#'
#' @name modern_plotting
NULL

# Global variable declarations to avoid R CMD check warnings
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "weight", "nodes", "node_size", "is_ego", "same_segment", "node_name",
    "level_name", "mobility_rate", "name", "segment", "x", "y",
    "Membership", "within.mobility", "Nodes", "Density", "share.of.mobility",
    "share.of.total", "Level_Metric", "Level", "Metric", "Value", "Max.path",
    "color", "x1", "y1", "x2", "y2", "node_id", "label"
  ))
}

# Helper function for NULL default values
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Modern Network Visualization for MONECA Results
#'
#' Creates sophisticated network visualizations of MONECA clustering results using
#' ggraph and ggplot2. This function provides modern, highly customizable plots
#' with support for multiple layout algorithms, node aesthetics, and segment highlighting.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param level Integer vector specifying which hierarchical levels to visualize.
#'   Default displays all levels except the first (which represents individual categories).
#' @param layout Character string or matrix specifying the layout algorithm:
#'   \itemize{
#'     \item "fr" (default): Fruchterman-Reingold force-directed layout
#'     \item "kk": Kamada-Kawai layout
#'     \item "dh": Davidson-Harel layout  
#'     \item "mds": Multidimensional scaling
#'     \item "stress": Stress majorization
#'     \item Matrix: Custom coordinate matrix (n_nodes x 2)
#'   }
#' @param edges Edge matrix or "auto" to automatically generate using 
#'   \code{\link{segment.edges}}. Default is "auto".
#' @param node_size Aesthetic mapping for node size:
#'   \itemize{
#'     \item "total": Size by total mobility volume (default)
#'     \item "mobility": Size by off-diagonal mobility rate
#'     \item Numeric vector: Custom sizes for each node
#'     \item Single numeric: Fixed size for all nodes
#'   }
#' @param node_color Aesthetic mapping for node color:
#'   \itemize{
#'     \item "segment" (default): Color by segment membership
#'     \item "mobility": Color by mobility rate
#'     \item Character vector: Custom colors for each node
#'     \item Single color: Fixed color for all nodes
#'   }
#' @param node_alpha Numeric value (0-1) for node transparency. Default is 0.8.
#' @param edge_width Aesthetic for edge width:
#'   \itemize{
#'     \item "weight" (default): Width proportional to edge weight
#'     \item Numeric: Fixed width for all edges
#'   }
#' @param edge_color Color for edges. Default is "grey50".
#' @param edge_alpha Numeric value (0-1) for edge transparency. Default is 0.6.
#' @param show_labels Logical indicating whether to display node labels. Default is TRUE.
#' @param label_size Numeric size for node labels. Default is 3.
#' @param show_segments Logical indicating whether to highlight segment boundaries.
#'   Default is TRUE.
#' @param segment_alpha Numeric value (0-1) for segment boundary transparency.
#'   Default is 0.3.
#' @param color_palette Character string specifying the color palette for segments.
#'   Can be any RColorBrewer palette name. Default is "Set3".
#' @param theme_style Character string specifying the plot theme:
#'   \itemize{
#'     \item "void" (default): Clean background with no axes
#'     \item "minimal": Minimal theme with subtle gridlines
#'     \item "classic": Traditional ggplot2 theme
#'   }
#' @param title Character string for plot title. Default is NULL (no title).
#' @param segment_naming Character string specifying the naming strategy for 
#'   segment labels. Options are "auto", "concat", "pattern", or "custom". 
#'   Default is "auto". See \code{\link{segment.membership.enhanced}} for details.
#' @param ... Additional arguments passed to ggraph layout functions.
#'
#' @return A ggplot2 object that can be further customized or displayed.
#'
#' @details
#' This function creates publication-quality network visualizations with extensive
#' customization options. It automatically handles node positioning, edge rendering,
#' and segment highlighting. The resulting plot can be further modified using
#' standard ggplot2 syntax.
#' 
#' For interactive exploration, different layout algorithms may work better with
#' different network structures. Force-directed layouts ("fr") work well for most
#' cases, while "stress" layouts often produce cleaner results for dense networks.
#'
#' @examples
#' # Generate synthetic data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#' 
#' # Basic network plot
#' plot_moneca_ggraph(seg)
#' 
#' # Customized plot with different aesthetics
#' plot_moneca_ggraph(seg,
#'   layout = "stress",
#'   node_color = "mobility",
#'   edge_width = "weight",
#'   color_palette = "Spectral",
#'   title = "Social Mobility Network",
#'   show_segments = FALSE
#' )
#' 
#' # Plot with custom node sizes and colors
#' custom_plot <- plot_moneca_ggraph(seg,
#'   node_size = c(8, 6, 10, 4, 7, 5),
#'   node_color = "red",
#'   edge_color = "darkblue",
#'   theme_style = "minimal"
#' )
#' 
#' # Further customize with ggplot2
#' custom_plot + 
#'   ggplot2::labs(subtitle = "Custom subtitle") +
#'   ggplot2::theme(plot.title = ggplot2::element_text(size = 16))
#' 
#' @seealso 
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_ego_ggraph}} for ego network visualization,
#' \code{\link{plot_stair_ggraph}} for multi-level visualization,
#' \code{\link{segment.edges}} for edge matrix generation
#' 
#' @export
plot_moneca_ggraph <- function(segments,
                              level = NULL,
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
                              segment_naming = "auto",
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
  
  # Validate segments object
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop("segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  
  # Set default level after validation
  if (is.null(level)) {
    level <- seq(segments$segment.list)[-1]
  }
  
  # Get edge matrix
  if (identical(edges, "auto")) {
    edge_matrix <- segment.edges(segments, level = 1)
  } else {
    edge_matrix <- edges
  }
  
  # Create igraph object with validation
  # Check if edge matrix has valid structure
  if (nrow(edge_matrix) == 0 || ncol(edge_matrix) == 0) {
    stop("Edge matrix is empty - cannot create network plot")
  }
  if (all(edge_matrix == 0, na.rm = TRUE)) {
    warning("Edge matrix contains no non-zero values - plot may be empty")
  }
  
  g <- moneca_graph_from_adjacency(edge_matrix, mode = "directed", weighted = TRUE)
  
  # Get node information
  n_nodes <- length(V(g))
  node_names <- V(g)$name
  if (is.null(node_names)) {
    node_names <- paste0("Node_", 1:n_nodes)
    V(g)$name <- node_names
  }
  
  # Convert to tidygraph
  tidy_graph <- tidygraph::as_tbl_graph(g)
  
  # Add node attributes
  mobility_matrix <- segments$mat.list[[1]]
  
  # Calculate node sizes
  if (identical(node_size, "total")) {
    node_totals <- (mobility_matrix[nrow(mobility_matrix), ] + 
                   mobility_matrix[, nrow(mobility_matrix)]) / 2
    node_totals <- node_totals[-length(node_totals)]
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = node_totals)
  } else if (identical(node_size, "mobility")) {
    # Calculate mobility rates (off-diagonal mobility)
    mobility_rates <- 1 - diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) / 
                     rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = mobility_rates)
  } else if (is.numeric(node_size) && length(node_size) == n_nodes) {
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = node_size)
  } else {
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(node_size = 5)
  }
  
  # Add segment membership with enhanced names
  if (identical(node_color, "segment")) {
    membership <- segment.membership.enhanced(segments, level = level, naming_strategy = segment_naming)
    # Match node names to membership
    node_segments <- membership$membership[match(node_names, membership$name)]
    node_segments[is.na(node_segments)] <- "Unassigned"
    
    # Get enhanced level names for better legends
    node_level_names <- membership$level_name[match(node_names, membership$name)]
    node_level_names[is.na(node_level_names)] <- "Unassigned"
    
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(
            segment = as.factor(node_segments),
            level_name = as.factor(node_level_names)
          )
  } else if (identical(node_color, "mobility")) {
    mobility_rates <- 1 - diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) / 
                     rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tidy_graph <- tidy_graph %>% tidygraph::activate(nodes) %>% 
          dplyr::mutate(mobility_rate = mobility_rates)
  }
  
  # Create base plot with validation
  if (is.character(layout)) {
    # Validate that the graph has nodes
    if (length(V(g)) == 0) {
      stop("Cannot create plot: graph has no nodes")
    }
    p <- ggraph::ggraph(tidy_graph, layout = layout)
  } else {
    # Validate layout matrix
    if (!is.matrix(layout) && !is.data.frame(layout)) {
      stop("Layout must be a matrix or data.frame with coordinates")
    }
    if (nrow(layout) != length(V(g))) {
      stop("Layout dimensions must match number of nodes in graph")
    }
    if (ncol(layout) < 2) {
      stop("Layout must have at least 2 columns (x, y coordinates)")
    }
    
    # Check for valid coordinates (not all the same, not infinite, not NA)
    x_coords <- layout[, 1]
    y_coords <- layout[, 2]
    
    if (all(is.na(x_coords)) || all(is.na(y_coords))) {
      stop("Layout coordinates cannot be all NA")
    }
    if (any(is.infinite(x_coords)) || any(is.infinite(y_coords))) {
      warning("Layout contains infinite coordinates, replacing with finite values")
      x_coords[is.infinite(x_coords)] <- 0
      y_coords[is.infinite(y_coords)] <- 0
    }
    if (length(unique(x_coords[!is.na(x_coords)])) == 1 && length(unique(y_coords[!is.na(y_coords)])) == 1) {
      warning("All layout coordinates are the same, adding small random jitter")
      x_coords <- x_coords + runif(length(x_coords), -0.1, 0.1)
      y_coords <- y_coords + runif(length(y_coords), -0.1, 0.1)
    }
    
    p <- ggraph::ggraph(tidy_graph, layout = "manual", x = x_coords, y = y_coords)
  }
  
  # Add segment boundaries (convex hulls) FIRST as background layer
  if (show_segments && identical(node_color, "segment")) {
    # Get the layout that ggraph is actually using
    if (is.character(layout)) {
      actual_layout <- ggraph::create_layout(tidy_graph, layout = layout)
    } else {
      # For manual layout, create layout data frame
      actual_layout <- data.frame(
        x = x_coords,
        y = y_coords,
        name = node_names,
        stringsAsFactors = FALSE
      )
      # Add all node attributes from tidy_graph
      node_data <- tidy_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::as_tibble()
      
      actual_layout <- merge(actual_layout, node_data, by = "name", all.x = TRUE)
    }
    
    # Add segment information to layout if not already present
    if (!"segment" %in% colnames(actual_layout)) {
      actual_layout$segment <- tidy_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::pull(segment) %>%
        as.character()
    }
    
    if (!"level_name" %in% colnames(actual_layout)) {
      actual_layout$level_name <- tidy_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::pull(level_name) %>%
        as.character()
    }
    
    # Create hull data
    hull_data <- actual_layout %>%
      dplyr::group_by(segment) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::do({
        if (nrow(.) >= 3) {
          hull_indices <- grDevices::chull(.$x, .$y)
          .[hull_indices, c("x", "y", "segment", "level_name")]
        } else {
          data.frame(x = numeric(0), y = numeric(0), segment = character(0), level_name = character(0))
        }
      }) %>%
      dplyr::ungroup()
    
    if (nrow(hull_data) > 0) {
      # Calculate label positions
      label_data <- hull_data %>%
        dplyr::group_by(segment) %>%
        dplyr::summarise(
          x = mean(x, na.rm = TRUE),
          y = max(y, na.rm = TRUE) + 0.15,  # Slightly higher label position
          label = dplyr::first(level_name),
          .groups = 'drop'
        )
      
      # Add hulls as the FIRST layer (background)
      hull_list <- split(hull_data, hull_data$segment)
      
      for (i in seq_along(hull_list)) {
        hull_segment <- hull_list[[i]]
        if (nrow(hull_segment) >= 3) {
          # Use ggforce if available for rounded hulls
          if (requireNamespace("ggforce", quietly = TRUE)) {
            p <- p + ggforce::geom_shape(
              data = hull_segment,
              ggplot2::aes(x = x, y = y),
              fill = RColorBrewer::brewer.pal(max(3, length(hull_list)), color_palette)[i],
              alpha = segment_alpha,
              expand = ggplot2::unit(0.02, "npc"),
              radius = ggplot2::unit(0.03, "npc"),
              show.legend = FALSE,
              inherit.aes = FALSE
            )
          } else {
            p <- p + ggplot2::geom_polygon(
              data = hull_segment,
              ggplot2::aes(x = x, y = y),
              fill = RColorBrewer::brewer.pal(max(3, length(hull_list)), color_palette)[i],
              alpha = segment_alpha,
              show.legend = FALSE,
              inherit.aes = FALSE
            )
          }
        }
      }
      
      # Store label data for later use
      hull_label_data <- label_data
    } else {
      hull_label_data <- NULL
    }
  } else {
    hull_label_data <- NULL
  }
  
  # Add edges (second layer)
  if (identical(edge_width, "weight")) {
    p <- p + ggraph::geom_edge_link(
      ggplot2::aes(width = weight, alpha = weight),
      color = edge_color
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.2, 2), guide = "none") +
    ggraph::scale_edge_alpha_continuous(range = c(0.2, 0.9), guide = "none")
  } else {
    edge_width_val <- if (is.numeric(edge_width)) edge_width else 0.5
    p <- p + ggraph::geom_edge_link(
      color = edge_color,
      alpha = edge_alpha,
      width = edge_width_val
    )
  }
  
  # Add nodes (third layer - on top)
  if (identical(node_color, "segment")) {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size, color = level_name),
      alpha = node_alpha,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_brewer(type = "qual", palette = color_palette, guide = "none")
  } else if (identical(node_color, "mobility")) {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size, color = mobility_rate),
      alpha = node_alpha,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_gradient(low = "blue", high = "red", guide = "none")
  } else {
    p <- p + ggraph::geom_node_point(
      ggplot2::aes(size = node_size),
      color = node_color,
      alpha = node_alpha,
      show.legend = FALSE
    )
  }
  
  # Add labels - only show individual node labels when not showing segment hulls
  if (show_labels && (!show_segments || !identical(node_color, "segment"))) {
    if (identical(node_color, "segment")) {
      p <- p + ggraph::geom_node_text(
        ggplot2::aes(label = level_name),
        size = label_size,
        repel = TRUE
      )
    } else {
      p <- p + ggraph::geom_node_text(
        ggplot2::aes(label = name),
        size = label_size,
        repel = TRUE
      )
    }
  }
  
  # Add segment labels if we have hull data
  if (!is.null(hull_label_data)) {
    p <- p + ggplot2::geom_text(
      data = hull_label_data,
      ggplot2::aes(x = x, y = y, label = label),
      size = 4,
      fontface = "bold",
      vjust = 0,
      inherit.aes = FALSE
    )
  }
  
  
  # Apply theme
  if (theme_style == "void") {
    p <- p + ggraph::theme_graph() +
           ggplot2::theme(
             plot.title = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 2))
           )
  } else if (theme_style == "minimal") {
    p <- p + ggplot2::theme_minimal() +
           ggplot2::theme(
             axis.text = ggplot2::element_blank(),
             axis.title = ggplot2::element_blank(),
             panel.grid = ggplot2::element_blank(),
             plot.title = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 2))
           )
  } else if (theme_style == "classic") {
    p <- p + ggplot2::theme_classic() +
           ggplot2::theme(
             axis.text = ggplot2::element_blank(),
             axis.title = ggplot2::element_blank(),
             plot.title = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 2))
           )
  }
  
  # Add title
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }
  
  # Scale node sizes without legend
  p <- p + ggplot2::scale_size_continuous(range = c(2, 8), guide = "none")
  
  return(p)
}

#' Ego Network Visualization with ggraph
#'
#' Creates focused visualizations of mobility patterns from a single focal node
#' (ego). This function shows all incoming and outgoing mobility flows for a
#' specific category, making it ideal for understanding individual position dynamics.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param mobility_matrix The original mobility matrix used in the moneca analysis.
#'   Should include row and column totals.
#' @param ego_id Integer or character specifying the focal node. Can be:
#'   \itemize{
#'     \item Integer: Row/column index in the mobility matrix
#'     \item Character: Row/column name from the mobility matrix
#'   }
#' @param min_weight Numeric threshold for minimum edge weight to include nodes.
#'   Only nodes connected to the ego with edge weights >= min_weight will be shown.
#'   Default is 0 (show all connected nodes). Use higher values to focus on 
#'   stronger mobility flows.
#' @param layout Character string specifying the layout algorithm. Default is "stress"
#'   which often works well for ego networks. Other options include "fr", "kk", "dh".
#' @param highlight_color Color for the ego (focal) node. Default is "red".
#' @param flow_color Character string specifying the color scheme for mobility flows.
#'   Default is "viridis". Can be any viridis variant ("viridis", "plasma", "inferno", etc.).
#' @param node_size_range Numeric vector of length 2 specifying the range for node sizes.
#'   Default is c(3, 12).
#' @param edge_width_range Numeric vector of length 2 specifying the range for edge widths.
#'   Default is c(0.5, 3).
#' @param title Character string for plot title. Default is NULL.
#' @param ... Additional arguments passed to the ggraph layout function.
#'
#' @return A ggplot2 object showing the ego network visualization.
#'
#' @details
#' Ego networks are particularly useful for understanding the mobility patterns
#' of specific social positions. The visualization highlights:
#' \itemize{
#'   \item The focal position (ego) in a distinct color
#'   \item Incoming mobility flows (edges pointing to ego)
#'   \item Outgoing mobility flows (edges from ego)
#'   \item The relative strength of different flows through edge width and color
#' }
#' 
#' Only non-zero mobility flows are displayed to reduce visual clutter.
#' Edge colors and widths are scaled to represent the volume of mobility flows.
#'
#' @examples
#' # Generate synthetic data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#' 
#' # Ego network for the middle category (index 3)
#' plot_ego_ggraph(seg, mobility_data, ego_id = 3, 
#'                 title = "Mobility from Middle Class")
#' 
#' # Ego network using category name (if available)
#' if (!is.null(rownames(mobility_data))) {
#'   plot_ego_ggraph(seg, mobility_data, ego_id = rownames(mobility_data)[1])
#' }
#' 
#' # Focus on strong mobility flows only (weight >= 10)
#' plot_ego_ggraph(seg, mobility_data, 
#'                 ego_id = 2,
#'                 min_weight = 10,
#'                 title = "Strong Mobility Flows")
#' 
#' # Customized ego plot
#' plot_ego_ggraph(seg, mobility_data, 
#'                 ego_id = 2,
#'                 layout = "fr",
#'                 highlight_color = "orange",
#'                 flow_color = "plasma",
#'                 edge_width_range = c(1, 5),
#'                 title = "Professional Class Mobility")
#' 
#' @seealso 
#' \code{\link{plot_moneca_ggraph}} for full network visualization,
#' \code{\link{plot_stair_ggraph}} for multi-level visualization,
#' \code{\link{moneca}} for the main analysis function
#' 
#' @export
plot_ego_ggraph <- function(segments,
                           mobility_matrix,
                           ego_id,
                           min_weight = 0,
                           layout = "stress",
                           highlight_color = "red",
                           flow_color = "viridis",
                           node_size_range = c(2, 8),
                           edge_width_range = c(0.2, 3),
                           title = NULL,
                           segment_naming = "auto",
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
  
  # Apply weight threshold - set edges below min_weight to 0
  ego_matrix[ego_matrix < min_weight] <- 0
  
  # Identify nodes that have connections above the threshold
  connected_nodes <- which(rowSums(ego_matrix) > 0 | colSums(ego_matrix) > 0)
  
  # Always include the ego node even if it has no connections above threshold
  connected_nodes <- unique(c(ego_id, connected_nodes))
  connected_nodes <- sort(connected_nodes)
  
  # Filter matrix to only include connected nodes
  filtered_matrix <- ego_matrix[connected_nodes, connected_nodes, drop = FALSE]
  
  # Create graph from filtered matrix
  g <- moneca_graph_from_adjacency(filtered_matrix, mode = "directed", weighted = TRUE)
  
  # Convert to tidygraph and filter out zero-weight edges
  ego_network <- tidygraph::as_tbl_graph(g)
  ego_network <- ego_network %>% 
    tidygraph::activate(edges) %>%
    dplyr::filter(weight > 0)
  
  # Add node attributes
  node_names <- V(g)$name
  if (is.null(node_names)) {
    # Use names from the filtered connected nodes
    if (!is.null(rownames(core_matrix))) {
      node_names <- rownames(core_matrix)[connected_nodes]
    } else {
      node_names <- paste0("Node_", connected_nodes)
    }
  }
  
  # Node sizes based on total mobility (for connected nodes only)
  node_totals_full <- rowSums(core_matrix) + colSums(core_matrix)
  node_totals <- node_totals_full[connected_nodes]
  
  # Find which filtered node is the ego
  ego_position_in_filtered <- which(connected_nodes == ego_id)
  
  # Get enhanced segment membership for all nodes
  membership <- segment.membership.enhanced(segments, level = 1:length(segments$segment.list), naming_strategy = segment_naming)
  
  # Find ego's segment
  ego_segment <- membership$membership[ego_id]
  ego_level_name <- membership$level_name[ego_id]
  
  # Determine which nodes belong to the same segment as ego
  same_segment_as_ego <- membership$membership[connected_nodes] == ego_segment
  
  # Get level names for connected nodes
  connected_level_names <- membership$level_name[connected_nodes]
  
  ego_network <- ego_network %>% tidygraph::activate(nodes) %>%
        dplyr::mutate(
          node_size = node_totals,
          is_ego = 1:tidygraph::graph_order() == ego_position_in_filtered,
          same_segment = same_segment_as_ego,
          node_name = node_names,
          level_name = connected_level_names
        )
  
  # Create plot with validation
  if (length(V(g)) == 0) {
    stop("Cannot create ego plot: no nodes connected to ego node")
  }
  if (length(E(g)) == 0) {
    warning("Ego plot has no edges - ego node may be isolated")
  }
  
  p <- ggraph::ggraph(ego_network, layout = layout)
  
  # Add edges with width based on flow volume
  p <- p + ggraph::geom_edge_link(
    ggplot2::aes(width = weight, color = weight, alpha = weight),
    end_cap = ggraph::circle(0.05, "cm")
  ) +
  ggraph::scale_edge_width_continuous(range = edge_width_range, guide = "none") +
  ggraph::scale_edge_color_viridis(name = "Flow Volume", guide = "none") +
  ggraph::scale_edge_alpha_continuous(range = c(0.3, 0.9), guide = "none")
  
  # Add nodes with coloring based on segment membership
  p <- p + ggraph::geom_node_point(
    ggplot2::aes(size = node_size, color = interaction(is_ego, same_segment)),
    alpha = 0.8
  ) +
  ggplot2::scale_color_manual(
    values = c("FALSE.FALSE" = "steelblue", 
               "FALSE.TRUE" = highlight_color, 
               "TRUE.FALSE" = highlight_color,
               "TRUE.TRUE" = highlight_color),
    guide = "none"
  ) +
  ggplot2::scale_size_continuous(range = node_size_range, name = "Total Mobility")
  
  # Add labels - use individual node names (professions)
  p <- p + ggraph::geom_node_text(
    ggplot2::aes(label = node_name),
    size = 3,
    repel = TRUE
  )
  
  # Apply theme with reduced title spacing
  p <- p + ggraph::theme_graph() +
         ggplot2::theme(
           plot.title = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 2))
         )
  
  # Add title
  if (is.null(title)) {
    ego_name <- node_names[ego_position_in_filtered]
    if (min_weight > 0) {
      title <- paste("Mobility Network for", ego_name, 
                     paste0("(min weight: ", min_weight, ")"))
    } else {
      title <- paste("Mobility Network for", ego_name)
    }
  }
  p <- p + ggplot2::ggtitle(title)
  
  return(p)
}

#' Multi-Level Segmentation Visualization (Stair Plot)
#'
#' Creates a series of network plots showing how segmentation evolves across
#' hierarchical levels in a moneca analysis. This "stair plot" provides insight
#' into the progressive clustering of social positions.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param levels Integer vector specifying which hierarchical levels to visualize.
#'   Default includes all levels.
#' @param layout Layout specification for consistency across plots. Can be:
#'   \itemize{
#'     \item NULL (default): Use layout.matrix() for consistent positioning
#'     \item Character string: Layout algorithm name ("fr", "kk", "stress", etc.)
#'     \item Matrix: Custom coordinate matrix for node positions
#'   }
#' @param ncol Integer specifying the number of columns in the plot grid.
#'   Default is 2. Set to 1 for vertical arrangement.
#' @param segment_naming Character string specifying the naming strategy for 
#'   segment labels. Default is "auto".
#' @param include_first_level Logical indicating whether to include the first level
#'   (individual classes without segmentation). Default is TRUE.
#' @param ... Additional arguments passed to \code{\link{plot_moneca_ggraph}}.
#'
#' @return 
#' If \code{combine_plots = TRUE}, returns a combined plot grid object.
#' If \code{combine_plots = FALSE}, returns a list of ggplot objects, one for each level.
#'
#' @details
#' The stair plot helps visualize the hierarchical nature of MONECA segmentation
#' by showing how larger segments at higher levels break down into smaller, more
#' specific segments at lower levels. This is particularly useful for:
#' \itemize{
#'   \item Understanding the segmentation process
#'   \item Identifying optimal levels of analysis
#'   \item Presenting results to different audiences
#'   \item Comparing segmentation stability across levels
#' }
#' 
#' When using a consistent layout across all plots, the relative positions of
#' nodes remain the same, making it easier to track how segments evolve.
#'
#' @examples
#' # Generate synthetic data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 4)
#' 
#' # Basic stair plot
#' stair_plots <- plot_stair_ggraph(seg)
#' 
#' # Customized stair plot with specific levels
#' custom_stair <- plot_stair_ggraph(seg, 
#'                                  levels = c(2, 3),
#'                                  layout = "stress",
#'                                  ncol = 1,
#'                                  node_color = "mobility")
#' 
#' # Return individual plots for further customization
#' plot_list <- plot_stair_ggraph(seg, combine_plots = FALSE)
#' # Modify individual plots
#' plot_list[[1]] <- plot_list[[1]] + ggplot2::labs(subtitle = "Level 2")
#' 
#' \dontrun{
#' # Display the combined plot
#' print(stair_plots)
#' 
#' # Save individual plots
#' for (i in seq_along(plot_list)) {
#'   ggplot2::ggsave(paste0("level_", i, ".png"), plot_list[[i]])
#' }
#' }
#' 
#' @seealso 
#' \code{\link{plot_moneca_ggraph}} for single-level visualization,
#' \code{\link{plot_ego_ggraph}} for ego network analysis,
#' \code{\link{layout.matrix}} for consistent layouts,
#' \code{\link{moneca}} for the main analysis function
#' 
#' @export
plot_stair_ggraph <- function(segments,
                             levels = seq_along(segments$segment.list),
                             layout = NULL,
                             ncol = 2,
                             segment_naming = "auto",
                             include_first_level = TRUE,
                             ...) {
  
  # Validate segments object
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop("segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function.")
  }
  
  # Create consistent layout if not provided
  if (is.null(layout)) {
    # Use the layout.matrix function from MONECA
    layout <- layout.matrix(segments)
  }
  
  plots <- list()
  
  # Add first level plot (individual nodes) if requested
  if (include_first_level) {
    # Extract title from ... if provided, otherwise use default
    dots <- list(...)
    if ("title" %in% names(dots)) {
      # User provided a title - don't override it
      p_first <- plot_moneca_ggraph(
        segments,
        level = 1,
        layout = layout,
        segment_naming = segment_naming,
        show_segments = FALSE,  # No hulls for individual level
        show_labels = TRUE,
        ...
      )
    } else {
      # Use default title
      p_first <- plot_moneca_ggraph(
        segments,
        level = 1,
        layout = layout,
        segment_naming = segment_naming,
        show_segments = FALSE,  # No hulls for individual level
        show_labels = TRUE,
        title = "Level 1: Individual Classes",
        ...
      )
    }
    
    # Remove all legends
    p_first <- p_first + ggplot2::theme(legend.position = "none")
    
    plots[[1]] <- p_first
    plot_names <- c("Level 1")
    
    # Adjust levels to start from 2
    plot_levels <- levels[levels > 1]
    start_idx <- 2
  } else {
    plot_levels <- levels
    start_idx <- 1
    plot_names <- character(0)
  }
  
  # Add subsequent level plots
  for (i in seq_along(plot_levels)) {
    level_idx <- plot_levels[i]
    
    # Extract title from ... if provided, otherwise use default
    dots <- list(...)
    if ("title" %in% names(dots)) {
      # User provided a title - don't override it
      p <- plot_moneca_ggraph(
        segments,
        level = 1:level_idx,
        layout = layout,
        segment_naming = segment_naming,
        ...
      )
    } else {
      # Use default title
      p <- plot_moneca_ggraph(
        segments,
        level = 1:level_idx,
        layout = layout,
        segment_naming = segment_naming,
        title = paste("Level", level_idx, "Segmentation"),
        ...
      )
    }
    
    # Remove all legends - hulls are now handled by plot_moneca_ggraph() in proper layer order
    p <- p + ggplot2::theme(legend.position = "none")
    
    plots[[start_idx + i - 1]] <- p
    plot_names <- c(plot_names, paste("Level", level_idx))
  }
  
  names(plots) <- plot_names
  
  return(plots)
}

#' Plot MONECA Results as Dendrogram
#'
#' Creates a dendrogram-like visualization of the hierarchical clustering results
#' from moneca analysis. This function shows how categories are progressively
#' aggregated across segmentation levels, making the hierarchical structure clear.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param height_method Character string specifying how to calculate dendrogram heights:
#'   \itemize{
#'     \item "uniform" (default): Equal spacing between levels
#'     \item "mobility": Height based on mobility reduction between levels
#'     \item "segments": Height based on number of segments at each level
#'   }
#' @param color_segments Logical indicating whether to color branches by final
#'   segment membership. Default is TRUE.
#' @param show_labels Logical indicating whether to show category labels at the
#'   bottom. Default is TRUE.
#' @param label_size Numeric size for labels. Default is 3.
#' @param branch_width Numeric width for dendrogram branches. Default is 1.
#' @param title Character string for plot title. Default is "MONECA Hierarchical Clustering".
#' @param subtitle Character string for plot subtitle. Default is NULL.
#' @param color_palette Character string specifying the RColorBrewer palette for
#'   segment colors. Default is "Set3".
#' @param theme_style Character string specifying the plot theme. Options are
#'   "minimal" (default), "classic", or "void".
#' @param vertical Logical indicating whether to plot vertically (TRUE, default)
#'   or horizontally (FALSE).
#'
#' @return A ggplot2 object representing the dendrogram.
#'
#' @details
#' This function creates a dendrogram visualization that clearly shows:
#' \itemize{
#'   \item How individual categories (leaves) are grouped at each level
#'   \item The hierarchical relationships between segments
#'   \item The progressive aggregation from individual categories to larger segments
#' }
#' 
#' The dendrogram branches show merging points where categories or segments are
#' combined based on the MONECA algorithm's clique detection. Unlike traditional
#' hierarchical clustering, MONECA can create non-binary trees where multiple
#' categories merge simultaneously.
#' 
#' \strong{Visual Features:}
#' \itemize{
#'   \item \strong{Curved branches}: Uses smooth curves instead of angular segments for better aesthetics
#'   \item \strong{Horizontal layout}: Levels progress horizontally (left to right) with categories arranged vertically
#'   \item \strong{No-Crossing Layout}: Categories are automatically ordered using a hierarchical algorithm that completely eliminates line crossings between all levels
#' }
#' 
#' The algorithm processes segments from the most aggregated level down,
#' ensuring that at each level, categories are grouped optimally to prevent any
#' crossing lines. This creates the clearest possible visualization of the 
#' hierarchical structure.
#'
#' @examples
#' # Generate synthetic data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#' 
#' # Basic dendrogram
#' plot_moneca_dendrogram(seg)
#' 
#' # Dendrogram with mobility-based heights
#' plot_moneca_dendrogram(seg, height_method = "mobility", 
#'                       title = "Mobility-based Hierarchical Clustering")
#' 
#' # Horizontal dendrogram without colors
#' plot_moneca_dendrogram(seg, vertical = FALSE, color_segments = FALSE)
#' 
#' # Customize appearance
#' plot_moneca_dendrogram(seg, 
#'                       color_palette = "Dark2",
#'                       branch_width = 1.5,
#'                       label_size = 4,
#'                       theme_style = "classic")
#'
#' @seealso 
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_moneca_ggraph}} for network visualization,
#' \code{\link{plot_stair_ggraph}} for multi-level visualization
#'
#' @export
plot_moneca_dendrogram <- function(segments,
                                  height_method = "uniform",
                                  color_segments = TRUE,
                                  show_labels = TRUE,
                                  label_size = 3,
                                  branch_width = 1,
                                  title = "MONECA Hierarchical Clustering",
                                  subtitle = NULL,
                                  color_palette = "Set3",
                                  theme_style = "minimal",
                                  vertical = TRUE) {
  
  # Input validation
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  
  # Extract segment list and original names
  seg_list <- segments$segment.list
  n_levels <- length(seg_list)
  
  # Get original category names
  mat <- segments$mat.list[[1]]
  cat_names <- rownames(mat)[-nrow(mat)]  # Remove "Total" row
  n_categories <- length(cat_names)
  
  # Initialize data structures for dendrogram
  # Note: x and y are flipped - x represents height (levels), y represents position
  edges <- data.frame(from = character(), to = character(), 
                     x1 = numeric(), y1 = numeric(), 
                     x2 = numeric(), y2 = numeric(),
                     level = integer(), segment = character(),
                     stringsAsFactors = FALSE)
  
  # Calculate heights for each level
  heights <- switch(height_method,
    "uniform" = seq(0, n_levels - 1),
    "mobility" = {
      # Calculate mobility reduction at each level
      mob_rates <- sapply(seq_len(n_levels), function(i) {
        mat <- segments$mat.list[[i]]
        l <- nrow(mat)
        if (l <= 1) return(1)
        diag_sum <- sum(diag(mat)[-l])
        total_sum <- sum(mat[-l, -l])
        if (total_sum == 0) return(1)
        diag_sum / total_sum
      })
      cumsum(c(0, diff(mob_rates)))
    },
    "segments" = {
      # Height based on number of segments
      n_segs <- sapply(seg_list, length)
      cumsum(c(0, -diff(log(n_segs + 1))))
    },
    stop("Invalid height_method. Choose 'uniform', 'mobility', or 'segments'")
  )
  
  # Normalize heights to 0-1 range
  if (length(heights) > 1 && max(heights) > min(heights)) {
    heights <- (heights - min(heights)) / (max(heights) - min(heights))
  }
  
  # Create node positions for each level
  node_positions <- list()
  
  # Determine optimal x-axis ordering to completely eliminate line crossings
  # Algorithm: Process levels from most aggregated to least aggregated,
  # grouping categories by their segment membership at each level
  # This guarantees no crossing lines between any adjacent levels
  if (n_levels > 1) {
    # Start with the highest level (most aggregated) and work down
    ordered_cats <- seq_len(n_categories)
    
    # Process levels from most aggregated to least aggregated
    for (level in n_levels:2) {
      segments_at_level <- seg_list[[level]]
      
      if (length(segments_at_level) == 0) next
      
      # Create new ordering based on this level's segments
      new_order <- integer(0)
      
      # Add categories in segment order
      for (seg_idx in seq_along(segments_at_level)) {
        segment_members <- segments_at_level[[seg_idx]]
        # Sort members within segment by their current order
        segment_members_ordered <- segment_members[order(match(segment_members, ordered_cats))]
        new_order <- c(new_order, segment_members_ordered)
      }
      
      # Add any missing categories (those not in any segment at this level) at the end
      missing_cats <- setdiff(ordered_cats, new_order)
      if (length(missing_cats) > 0) {
        # Sort missing categories by their current order
        missing_cats_ordered <- missing_cats[order(match(missing_cats, ordered_cats))]
        new_order <- c(new_order, missing_cats_ordered)
      }
      
      # Update the ordering
      ordered_cats <- new_order
    }
  } else {
    # If only one level, keep original order
    ordered_cats <- seq_len(n_categories)
  }
  
  # Level 1: Individual categories with optimized positions
  # Note: coordinates flipped - x is now height (level), y is position
  node_positions[[1]] <- data.frame(
    node_id = paste0("L1_", seq_len(n_categories)),
    label = cat_names,
    x = heights[1], # Height (level) - now on x-axis
    y = match(seq_len(n_categories), ordered_cats), # Position - now on y-axis
    level = 1,
    segment_id = seq_len(n_categories),
    original_index = seq_len(n_categories), # Keep track of original indices
    stringsAsFactors = FALSE
  )
  
  # Process each subsequent level
  for (level in 2:n_levels) {
    segments_at_level <- seg_list[[level]]
    n_segments <- length(segments_at_level)
    
    if (n_segments == 0) next
    
    # Calculate y positions as centers of constituent nodes  
    # Note: coordinates flipped - y positions are now the main positioning axis
    y_positions <- numeric(n_segments)
    segment_labels <- character(n_segments)
    
    for (i in seq_len(n_segments)) {
      members <- segments_at_level[[i]]
      # Find y positions of members from level 1
      member_y <- node_positions[[1]]$y[members]
      y_positions[i] <- mean(member_y)
      
      # Create segment label
      if (length(members) <= 3) {
        segment_labels[i] <- paste(cat_names[members], collapse = "-")
      } else {
        segment_labels[i] <- paste0("S", level, ".", i, " (", length(members), " nodes)")
      }
    }
    
    node_positions[[level]] <- data.frame(
      node_id = paste0("L", level, "_", seq_len(n_segments)),
      label = segment_labels,
      x = heights[level], # Height (level) - now on x-axis
      y = y_positions,    # Position - now on y-axis
      level = level,
      segment_id = seq_len(n_segments),
      original_index = NA, # Add consistent column structure
      stringsAsFactors = FALSE
    )
    
    # Create edges from previous level to current level
    for (i in seq_len(n_segments)) {
      members <- segments_at_level[[i]]
      
      # Find which nodes from previous level map to this segment
      if (level == 2) {
        # Direct mapping from categories
        prev_nodes <- node_positions[[1]][members, ]
      } else {
        # Find which previous segments contain these members
        prev_segments <- seg_list[[level - 1]]
        prev_indices <- which(sapply(prev_segments, function(ps) {
          any(members %in% ps)
        }))
        prev_nodes <- node_positions[[level - 1]][prev_indices, ]
      }
      
      current_node <- node_positions[[level]][i, ]
      
      # Create edges with flipped coordinates
      for (j in seq_len(nrow(prev_nodes))) {
        new_edge <- data.frame(
          from = prev_nodes$node_id[j],
          to = current_node$node_id,
          x1 = prev_nodes$x[j],  # Previous level height
          y1 = prev_nodes$y[j],  # Previous node position
          x2 = current_node$x,   # Current level height
          y2 = current_node$y,   # Current node position
          level = level,
          segment = paste0("S", level, ".", i),
          stringsAsFactors = FALSE
        )
        edges <- rbind(edges, new_edge)
      }
    }
  }
  
  # Combine all node positions
  all_nodes <- do.call(rbind, node_positions)
  
  # Assign colors based on final segment membership
  if (color_segments && n_levels > 1) {
    final_segments <- seg_list[[n_levels]]
    n_final_segments <- length(final_segments)
    
    # Get colors from palette
    if (n_final_segments <= 12) {
      segment_colors <- RColorBrewer::brewer.pal(max(3, n_final_segments), color_palette)
    } else {
      segment_colors <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(12, color_palette)
      )(n_final_segments)
    }
    
    # Assign colors to edges based on final segment membership
    edge_colors <- character(nrow(edges))
    for (i in seq_len(nrow(edges))) {
      # Find which final segment this edge belongs to
      edge_level <- edges$level[i]
      if (edge_level == n_levels) {
        # Direct assignment for final level
        seg_idx <- as.numeric(gsub("S\\d+\\.(\\d+)", "\\1", edges$segment[i]))
        edge_colors[i] <- segment_colors[seg_idx]
      } else {
        # Trace forward to find final segment
        # For simplicity, use black for intermediate levels
        edge_colors[i] <- "black"
      }
    }
    edges$color <- edge_colors
  } else {
    edges$color <- "black"
  }
  
  # Create the plot
  p <- ggplot2::ggplot()
  
  # Add edges (dendrogram branches) using curves
  if (nrow(edges) > 0) {
    p <- p + ggplot2::geom_curve(
      data = edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, color = color),
      size = branch_width,
      curvature = 0.2,  # Moderate curve
      angle = 90,       # Curve angle
      ncp = 5,          # Number of control points
      lineend = "round"
    )
  }
  
  # Add node points
  p <- p + ggplot2::geom_point(
    data = all_nodes,
    ggplot2::aes(x = x, y = y),
    size = 3,
    color = "black"
  )
  
  # Add labels for bottom level (in optimized order)
  if (show_labels) {
    bottom_nodes <- all_nodes[all_nodes$level == 1, ]
    # Sort by y position to ensure labels appear in the correct order (coordinates flipped)
    bottom_nodes <- bottom_nodes[order(bottom_nodes$y), ]
    # Update labels to show categories in their new positions
    bottom_nodes$label <- cat_names[ordered_cats]
    
    p <- p + ggplot2::geom_text(
      data = bottom_nodes,
      ggplot2::aes(x = x, y = y, label = label),
      hjust = -0.1,  # Position to the left of the leftmost points (since x is now height)
      size = label_size,
      angle = if (vertical) 0 else 45,  # Adjust angles for flipped orientation
      vjust = 0.5
    )
  }
  
  # Apply theme
  p <- p + switch(theme_style,
    "minimal" = ggplot2::theme_minimal(),
    "classic" = ggplot2::theme_classic(),
    "void" = ggplot2::theme_void(),
    ggplot2::theme_minimal()
  )
  
  # Customize theme
  p <- p + ggplot2::theme(
    legend.position = "none",
    axis.text = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
  )
  
  # Add titles
  p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  
  # Set scale to use actual colors
  if (color_segments) {
    p <- p + ggplot2::scale_color_identity()
  }
  
  # Since coordinates are now flipped by default (x=height, y=position),
  # the vertical parameter behavior is reversed
  if (vertical) {
    p <- p + ggplot2::coord_flip()
  }
  
  # Expand limits slightly (adjust for flipped coordinates)
  if (vertical) {
    p <- p + ggplot2::expand_limits(
      x = c(min(heights) - 0.1, max(heights) + 0.1)
    )
  } else {
    p <- p + ggplot2::expand_limits(
      y = c(min(heights) - 0.1, max(heights) + 0.1)
    )
  }
  
  return(p)
}

#' Visualize Segment Quality Metrics
#'
#' Creates comprehensive visualizations of segment quality metrics from MONECA 
#' analysis. This function provides multiple plot types to help assess the 
#' quality and characteristics of the segmentation.
#'
#' @param segments A moneca object returned by \code{\link{moneca}}.
#' @param plot_type Character string specifying the type of visualization:
#'   \itemize{
#'     \item "overview" (default): Multi-panel overview of key metrics
#'     \item "cohesion": Within-mobility vs segment size scatter plot
#'     \item "radar": Radar/spider plot of normalized metrics per segment
#'     \item "heatmap": Heatmap of all metrics across levels
#'     \item "evolution": Line plot showing metric evolution across levels
#'   }
#' @param level Integer specifying which hierarchical level to visualize. 
#'   Default is 2. Use NULL for all levels (only valid for some plot types).
#' @param metrics Character vector of metrics to include. Default includes all.
#'   Options: "within.mobility", "share.of.mobility", "Density", "Nodes", 
#'   "Max.path", "share.of.total"
#' @param color_palette Character string specifying the RColorBrewer palette.
#'   Default is "Set3" for categorical data, "RdYlBu" for continuous.
#' @param theme_style Character string specifying the plot theme: "minimal" 
#'   (default), "classic", or "void".
#' @param title Character string for plot title. Default is auto-generated
#'   based on plot type.
#' @param show_labels Logical indicating whether to show segment labels.
#'   Default is TRUE.
#' @param label_size Numeric size for labels. Default is 3.
#'
#' @return A ggplot2 object (or list of ggplot2 objects for "overview" type).
#'
#' @details
#' Each plot type serves a different analytical purpose:
#' 
#' \strong{Overview}: Four-panel display showing:
#' \itemize{
#'   \item Within-mobility by segment (bar chart)
#'   \item Segment sizes (nodes) 
#'   \item Network density
#'   \item Share of total mobility
#' }
#' 
#' \strong{Cohesion}: Scatter plot with within-mobility on y-axis and segment
#' size on x-axis. Ideal segments appear in the upper-right (large and cohesive).
#' Points are sized by share of mobility.
#' 
#' \strong{Radar}: Multi-dimensional comparison showing all metrics normalized
#' to 0-1 scale. Useful for comparing segment profiles.
#' 
#' \strong{Heatmap}: Shows all metrics across all levels and segments. Colors
#' indicate metric values, making patterns easy to spot.
#' 
#' \strong{Evolution}: Line plots showing how each metric changes across 
#' hierarchical levels for each segment lineage.
#'
#' @examples
#' # Generate data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#' 
#' # Overview of level 2 quality
#' plot_segment_quality(seg)
#' 
#' # Cohesion vs size analysis
#' plot_segment_quality(seg, plot_type = "cohesion", level = 2)
#' 
#' # Radar plot for segment comparison
#' plot_segment_quality(seg, plot_type = "radar", level = 2)
#' 
#' # Heatmap of all metrics across levels
#' plot_segment_quality(seg, plot_type = "heatmap", level = NULL)
#' 
#' # Evolution of metrics across levels
#' plot_segment_quality(seg, plot_type = "evolution", 
#'                     metrics = c("within.mobility", "Density"))
#' 
#' # Custom styling
#' plot_segment_quality(seg, plot_type = "cohesion",
#'                     color_palette = "Dark2",
#'                     theme_style = "classic",
#'                     title = "Segment Quality Analysis")
#'
#' @seealso 
#' \code{\link{segment.quality}} for the underlying metrics,
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_moneca_ggraph}} for network visualization
#'
#' @export
plot_segment_quality <- function(segments,
                                plot_type = "overview",
                                level = 2,
                                metrics = NULL,
                                color_palette = NULL,
                                theme_style = "minimal",
                                title = NULL,
                                show_labels = TRUE,
                                label_size = 3) {
  
  # Input validation
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  
  # Get quality data
  quality_data <- segment.quality(segments, final.solution = FALSE)
  
  # Set default metrics if not provided
  if (is.null(metrics)) {
    metrics <- c("within.mobility", "share.of.mobility", "Density", 
                "Nodes", "Max.path", "share.of.total")
  }
  
  # Set default color palette based on plot type
  if (is.null(color_palette)) {
    color_palette <- if (plot_type %in% c("heatmap", "cohesion")) "RdYlBu" else "Set3"
  }
  
  # Apply theme
  theme_base <- switch(theme_style,
    "minimal" = ggplot2::theme_minimal(),
    "classic" = ggplot2::theme_classic(),
    "void" = ggplot2::theme_void(),
    ggplot2::theme_minimal()
  )
  
  # Generate plots based on type
  if (plot_type == "overview") {
    # Multi-panel overview
    if (is.null(level)) level <- 2
    
    # Extract data for specified level
    level_cols <- grep(paste0("^", level, ": "), colnames(quality_data), value = TRUE)
    
    # Check if we found any columns for this level
    if (length(level_cols) == 0) {
      # Extract available levels from column names
      all_level_cols <- grep("^[0-9]+:", colnames(quality_data), value = TRUE)
      available_levels <- unique(gsub(":.*", "", all_level_cols))
      stop(paste("No data found for level", level, ". Available levels:", 
                 paste(available_levels, collapse = ", ")))
    }
    
    # Ensure we get a data frame even with single column
    level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
    
    # Clean column names
    colnames(level_data) <- gsub(paste0("^", level, ": "), "", colnames(level_data))
    
    # Create individual plots
    plots <- list()
    
    # 1. Within-mobility bar chart
    p1 <- ggplot2::ggplot(level_data, ggplot2::aes(x = Membership, y = within.mobility)) +
      ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
      ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "red", alpha = 0.5) +
      ggplot2::labs(x = "Segment", y = "Within-mobility", 
                   title = "Segment Cohesion") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    if (show_labels && nrow(level_data) <= 10) {
      p1 <- p1 + ggplot2::geom_text(ggplot2::aes(label = round(within.mobility, 2)), 
                                    vjust = -0.5, size = label_size)
    }
    plots[[1]] <- p1
    
    # 2. Segment size (nodes)
    p2 <- ggplot2::ggplot(level_data, ggplot2::aes(x = Membership, y = Nodes)) +
      ggplot2::geom_bar(stat = "identity", fill = "darkgreen") +
      ggplot2::labs(x = "Segment", y = "Number of Nodes", 
                   title = "Segment Size") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    if (show_labels && nrow(level_data) <= 10) {
      p2 <- p2 + ggplot2::geom_text(ggplot2::aes(label = Nodes), 
                                    vjust = -0.5, size = label_size)
    }
    plots[[2]] <- p2
    
    # 3. Network density
    p3 <- ggplot2::ggplot(level_data, ggplot2::aes(x = Membership, y = Density)) +
      ggplot2::geom_bar(stat = "identity", fill = "orange") +
      ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", alpha = 0.5) +
      ggplot2::labs(x = "Segment", y = "Network Density", 
                   title = "Internal Connectivity") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    if (show_labels && nrow(level_data) <= 10) {
      p3 <- p3 + ggplot2::geom_text(ggplot2::aes(label = round(Density, 2)), 
                                    vjust = -0.5, size = label_size)
    }
    plots[[3]] <- p3
    
    # 4. Share of mobility
    p4 <- ggplot2::ggplot(level_data, ggplot2::aes(x = Membership, y = share.of.mobility)) +
      ggplot2::geom_bar(stat = "identity", fill = "purple") +
      ggplot2::labs(x = "Segment", y = "Share of Total Mobility", 
                   title = "Relative Importance") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    if (show_labels && nrow(level_data) <= 10) {
      p4 <- p4 + ggplot2::geom_text(ggplot2::aes(label = round(share.of.mobility, 2)), 
                                    vjust = -0.5, size = label_size)
    }
    plots[[4]] <- p4
    
    # Combine plots
    combined <- gridExtra::grid.arrange(
      grobs = plots,
      ncol = 2,
      top = title %||% paste("Segment Quality Overview - Level", level)
    )
    
    return(combined)
    
  } else if (plot_type == "cohesion") {
    # Cohesion vs size scatter plot
    if (is.null(level)) level <- 2
    
    # Extract data for specified level
    level_cols <- grep(paste0("^", level, ": "), colnames(quality_data), value = TRUE)
    
    # Check if we found any columns for this level
    if (length(level_cols) == 0) {
      # Extract available levels from column names
      all_level_cols <- grep("^[0-9]+:", colnames(quality_data), value = TRUE)
      available_levels <- unique(gsub(":.*", "", all_level_cols))
      stop(paste("No data found for level", level, ". Available levels:", 
                 paste(available_levels, collapse = ", ")))
    }
    
    level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
    colnames(level_data) <- gsub(paste0("^", level, ": "), "", colnames(level_data))
    
    # Create scatter plot
    p <- ggplot2::ggplot(level_data, 
                        ggplot2::aes(x = Nodes, y = within.mobility, 
                                    size = share.of.mobility, color = Membership)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::scale_size_continuous(range = c(3, 10), name = "Share of Mobility") +
      ggplot2::geom_hline(yintercept = 0.7, linetype = "dashed", color = "red", alpha = 0.5) +
      ggplot2::geom_vline(xintercept = 2, linetype = "dashed", color = "blue", alpha = 0.5) +
      ggplot2::labs(x = "Number of Nodes", 
                   y = "Within-mobility (Cohesion)",
                   title = title %||% paste("Segment Quality Analysis - Level", level),
                   subtitle = "Ideal segments: upper-right quadrant") +
      theme_base
    
    # Add labels if requested
    if (show_labels) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = Membership), 
                                  vjust = -1, size = label_size)
    }
    
    # Color palette
    n_segments <- nrow(level_data)
    if (n_segments <= 12) {
      p <- p + ggplot2::scale_color_brewer(palette = color_palette)
    } else {
      p <- p + ggplot2::scale_color_manual(
        values = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(12, color_palette)
        )(n_segments)
      )
    }
    
    return(p)
    
  } else if (plot_type == "radar") {
    # Radar plot
    if (is.null(level)) level <- 2
    
    # Extract and prepare data
    level_cols <- grep(paste0("^", level, ": "), colnames(quality_data), value = TRUE)
    
    # Check if we found any columns for this level
    if (length(level_cols) == 0) {
      # Extract available levels from column names
      all_level_cols <- grep("^[0-9]+:", colnames(quality_data), value = TRUE)
      available_levels <- unique(gsub(":.*", "", all_level_cols))
      stop(paste("No data found for level", level, ". Available levels:", 
                 paste(available_levels, collapse = ", ")))
    }
    
    level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
    colnames(level_data) <- gsub(paste0("^", level, ": "), "", colnames(level_data))
    
    # Select only requested metrics
    metric_cols <- intersect(metrics, colnames(level_data))
    radar_data <- level_data[, c("Membership", metric_cols), drop = FALSE]
    
    # Normalize metrics to 0-1 scale
    for (col in metric_cols) {
      if (col != "Membership") {
        col_data <- radar_data[[col]]
        radar_data[[col]] <- (col_data - min(col_data, na.rm = TRUE)) / 
                            (max(col_data, na.rm = TRUE) - min(col_data, na.rm = TRUE))
      }
    }
    
    # Reshape data for plotting
    radar_long <- tidyr::pivot_longer(radar_data, 
                                     cols = -Membership, 
                                     names_to = "Metric", 
                                     values_to = "Value")
    
    # Create radar plot using coord_polar
    p <- ggplot2::ggplot(radar_long, 
                        ggplot2::aes(x = Metric, y = Value, group = Membership, 
                                    color = Membership)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::coord_polar() +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(title = title %||% paste("Segment Profiles - Level", level),
                   subtitle = "Normalized metrics (0-1 scale)") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10))
    
    # Color palette
    n_segments <- length(unique(radar_long$Membership))
    if (n_segments <= 12) {
      p <- p + ggplot2::scale_color_brewer(palette = color_palette)
    } else {
      p <- p + ggplot2::scale_color_manual(
        values = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(12, color_palette)
        )(n_segments)
      )
    }
    
    return(p)
    
  } else if (plot_type == "heatmap") {
    # Heatmap of all metrics
    
    # Select metrics columns
    all_metric_cols <- character()
    for (m in metrics) {
      cols <- grep(paste0(": ", m, "$"), colnames(quality_data), value = TRUE)
      all_metric_cols <- c(all_metric_cols, cols)
    }
    
    # Prepare data
    heatmap_data <- quality_data[, c("Membership", all_metric_cols), drop = FALSE]
    
    # Reshape to long format
    heatmap_long <- tidyr::pivot_longer(heatmap_data,
                                       cols = -Membership,
                                       names_to = "Level_Metric",
                                       values_to = "Value")
    
    # Separate level and metric
    heatmap_long$Level <- gsub(": .*", "", heatmap_long$Level_Metric)
    heatmap_long$Metric <- gsub("^[0-9]+: ", "", heatmap_long$Level_Metric)
    
    # Create heatmap
    p <- ggplot2::ggplot(heatmap_long, 
                        ggplot2::aes(x = Level, y = Membership, fill = Value)) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(~ Metric, scales = "free", ncol = 3) +
      ggplot2::scale_fill_distiller(palette = color_palette, direction = 1) +
      ggplot2::labs(title = title %||% "Segment Quality Metrics Across Levels",
                   x = "Hierarchical Level", y = "Segment") +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    
    return(p)
    
  } else if (plot_type == "evolution") {
    # Evolution across levels
    
    # Prepare data for evolution plot
    evolution_data <- list()
    
    for (m in metrics) {
      metric_cols <- grep(paste0(": ", m, "$"), colnames(quality_data), value = TRUE)
      if (length(metric_cols) > 0) {
        metric_data <- quality_data[, c("Membership", metric_cols), drop = FALSE]
        
        # Reshape to long format
        metric_long <- tidyr::pivot_longer(metric_data,
                                         cols = -Membership,
                                         names_to = "Level",
                                         values_to = "Value")
        metric_long$Level <- as.numeric(gsub(": .*", "", metric_long$Level))
        metric_long$Metric <- m
        
        evolution_data[[m]] <- metric_long
      }
    }
    
    # Combine all metrics
    evolution_combined <- do.call(rbind, evolution_data)
    
    # Create line plot
    p <- ggplot2::ggplot(evolution_combined, 
                        ggplot2::aes(x = Level, y = Value, color = Membership, 
                                    group = Membership)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
      ggplot2::labs(title = title %||% "Metric Evolution Across Hierarchical Levels",
                   x = "Hierarchical Level", y = "Metric Value") +
      theme_base
    
    # Color palette
    n_segments <- length(unique(evolution_combined$Membership))
    if (n_segments <= 12) {
      p <- p + ggplot2::scale_color_brewer(palette = color_palette)
    } else {
      p <- p + ggplot2::scale_color_manual(
        values = grDevices::colorRampPalette(
          RColorBrewer::brewer.pal(12, color_palette)
        )(n_segments)
      )
    }
    
    return(p)
    
  } else {
    stop("Invalid plot_type. Choose 'overview', 'cohesion', 'radar', 'heatmap', or 'evolution'")
  }
}