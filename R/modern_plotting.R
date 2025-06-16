#' Modern MONECA Plotting with ggraph
#'
#' Enhanced plotting functions using ggplot2 and ggraph for MONECA objects.
#' These functions replace the old plotting system with modern, customizable
#' network visualizations.
#'
#' @name modern_plotting
NULL

# Global variable declarations to avoid R CMD check warnings
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "weight", "nodes", "node_size", "is_ego", "same_segment", "node_name",
    "level_name", "mobility_rate", "name", "segment", "x", "y"
  ))
}

#' Modern Network Visualization for MONECA Results
#'
#' Creates sophisticated network visualizations of MONECA clustering results using
#' ggraph and ggplot2. This function provides modern, highly customizable plots
#' with support for multiple layout algorithms, node aesthetics, and segment highlighting.
#'
#' @param segments A MONECA object returned by \code{\link{moneca}}.
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
    stop("segments$mat.list is empty - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
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
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param mobility_matrix The original mobility matrix used in the MONECA analysis.
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
#' hierarchical levels in a MONECA analysis. This "stair plot" provides insight
#' into the progressive clustering of social positions.
#'
#' @param segments A MONECA object returned by \code{\link{moneca}}.
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
    stop("segments$mat.list is empty - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
  }
  if (is.null(segments$mat.list[[1]])) {
    stop("segments$mat.list[[1]] is NULL - the MONECA object appears to be incomplete. Please re-run the moneca() function.")
  }
  
  # Create consistent layout if not provided
  if (is.null(layout)) {
    # Use the layout.matrix function from MONECA
    layout <- layout.matrix(segments)
  }
  
  plots <- list()
  
  # Add first level plot (individual nodes) if requested
  if (include_first_level) {
    p_first <- plot_moneca_ggraph(
      segments,
      level = 1,
      layout = layout,
      title = "Level 1: Individual Classes",
      segment_naming = segment_naming,
      show_segments = FALSE,  # No hulls for individual level
      show_labels = TRUE,
      ...
    )
    
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
    
    p <- plot_moneca_ggraph(
      segments,
      level = 1:level_idx,
      layout = layout,
      title = paste("Level", level_idx, "Segmentation"),
      segment_naming = segment_naming,
      ...
    )
    
    # Remove all legends - hulls are now handled by plot_moneca_ggraph() in proper layer order
    p <- p + ggplot2::theme(legend.position = "none")
    
    plots[[start_idx + i - 1]] <- p
    plot_names <- c(plot_names, paste("Level", level_idx))
  }
  
  names(plots) <- plot_names
  
  return(plots)
}