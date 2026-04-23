#' Modern MONECA Plotting with ggraph
#'
#' Enhanced plotting functions using ggplot2 and ggraph for moneca objects.
#' These functions replace the old plotting system with modern, customizable
#' network visualizations.
#'
#' @name modern_plotting
#' @keywords internal
NULL

# Global variable declarations to avoid R CMD check warnings
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "weight",
    "nodes",
    "node_size",
    "is_ego",
    "same_segment",
    "node_name",
    "level_name",
    "mobility_rate",
    "name",
    "segment",
    "x",
    "y",
    "Membership",
    "within.mobility",
    "Nodes",
    "Density",
    "share.of.mobility",
    "share.of.total",
    "Level_Metric",
    "Level",
    "Metric",
    "Value",
    "Max.path",
    "color",
    "x1",
    "y1",
    "x2",
    "y2",
    "node_id",
    "label",
    "segment_label",
    "xend",
    "yend",
    "edge_color",
    "level_child",
    "orientation",
    "label_y",
    "pt_y",
    "seg_label"
  ))
}

# Helper function for NULL default values
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# 0. CVD-safe palette constants -----
.okabe_ito_palette <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#999999"
)

.overview_bar_colors <- c(
  cohesion = "#0072B2",
  size = "#E69F00",
  density = "#009E73",
  share = "#CC79A7"
)

.ref_line_high <- "#D55E00"
.ref_line_low <- "#0072B2"

# 1. Internal helper: get or compute metadata -----
.get_metadata <- function(segments) {
  if (!is.null(segments$segment_metadata)) {
    segments$segment_metadata
  } else {
    moneca_segments(segments)
  }
}

# 2. Internal helper: membership from metadata -----
.membership_from_metadata <- function(meta, level) {
  target_level <- max(level)
  if (target_level < 1 || target_level > meta$n_levels) {
    stop("level must be between 1 and ", meta$n_levels, call. = FALSE)
  }

  groups <- meta$levels[[target_level]]$groups
  map_df <- meta$levels[[target_level]]$map
  node_names <- meta$original_names
  n_nodes <- length(node_names)

  membership <- character(n_nodes)
  seg_label <- character(n_nodes)

  for (g in seq_along(groups)) {
    idx <- groups[[g]]
    membership[idx] <- paste0(target_level, ".", g)
    seg_label[idx] <- map_df$label[g]
  }

  data.frame(
    name = node_names,
    membership = membership,
    segment_label = seg_label,
    stringsAsFactors = FALSE
  )
}

# 3. Helper function to create segment labels based on segment_naming parameter -----
create_segment_labels <- function(
  segments,
  level,
  segment_numbers,
  segment_naming
) {
  # Get canonical metadata
  meta <- .get_metadata(segments)

  # Default labels from metadata
  if (level >= 1 && level <= meta$n_levels) {
    map_df <- meta$levels[[level]]$map
    segment_labels <- character(length(segment_numbers))
    for (i in seq_along(segment_numbers)) {
      seg_num <- segment_numbers[i]
      if (is.na(seg_num)) {
        segment_labels[i] <- paste0("Segment ", i)
      } else if (seg_num >= 1 && seg_num <= nrow(map_df)) {
        segment_labels[i] <- map_df$label[seg_num]
      } else {
        segment_labels[i] <- paste0("Segment ", seg_num)
      }
    }
  } else {
    segment_labels <- paste0("Segment ", segment_numbers)
  }

  # Override with custom dataframe if provided
  if (is.data.frame(segment_naming)) {
    if (!all(c("name", "segment_label") %in% colnames(segment_naming))) {
      stop(
        "segment_naming dataframe must have columns 'name' and 'segment_label'"
      )
    }
    for (i in seq_along(segment_labels)) {
      # Match by representative name (strip +N suffix)
      base_name <- sub("\\+[0-9]+$", "", segment_labels[i])
      custom_idx <- match(base_name, segment_naming$name)
      if (!is.na(custom_idx)) {
        segment_labels[i] <- segment_naming$segment_label[custom_idx]
      }
    }
  } else if (!is.null(segment_naming) && is.character(segment_naming)) {
    if (!segment_naming %in% c("auto", "concat", "pattern", "custom")) {
      stop(
        "segment_naming must be 'auto', 'concat', 'pattern', 'custom', a dataframe, or NULL"
      )
    }
  }

  segment_labels
}

# 4. Internal helper: compute unified segment colors -----
.compute_segment_colors <- function(meta, levels, color_palette) {
  all_labels <- character(0)
  for (lvl in levels) {
    membership <- .membership_from_metadata(meta, lvl)
    all_labels <- union(all_labels, unique(membership$segment_label))
  }
  n <- length(all_labels)

  cols <- .resolve_palette_colors(n, color_palette)

  setNames(cols, sort(all_labels))
}

# 4b. Internal helper: resolve n colors from a palette name -----
.resolve_palette_colors <- function(n, color_palette) {
  if (identical(color_palette, "okabe-ito")) {
    if (n <= length(.okabe_ito_palette)) {
      return(.okabe_ito_palette[seq_len(n)])
    }
    if (requireNamespace("viridis", quietly = TRUE)) {
      return(viridis::viridis(n))
    }
    return(scales::hue_pal()(n))
  }

  if (
    color_palette %in% c("viridis", "plasma", "inferno", "cividis", "magma")
  ) {
    if (!requireNamespace("viridis", quietly = TRUE)) {
      stop(
        "Package 'viridis' is required for viridis color palettes. Please install it."
      )
    }
    return(viridis::viridis(n, option = color_palette))
  }

  max_brew <- RColorBrewer::brewer.pal.info[color_palette, "maxcolors"]
  if (n <= max_brew) {
    RColorBrewer::brewer.pal(max(3, n), color_palette)[seq_len(n)]
  } else {
    grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(max_brew, color_palette)
    )(n)
  }
}

# 4c. Internal helper: apply discrete color scale to a ggplot -----
.apply_discrete_color_scale <- function(p, color_palette, n, guide = "none") {
  if (identical(color_palette, "okabe-ito")) {
    cols <- .resolve_palette_colors(n, color_palette)
    return(p + ggplot2::scale_color_manual(values = cols, guide = guide))
  }

  if (
    color_palette %in% c("viridis", "plasma", "inferno", "cividis", "magma")
  ) {
    return(
      p + ggplot2::scale_color_viridis_d(option = color_palette, guide = guide)
    )
  }

  if (n <= 12) {
    p + ggplot2::scale_color_brewer(palette = color_palette, guide = guide)
  } else {
    cols <- .resolve_palette_colors(n, color_palette)
    p + ggplot2::scale_color_manual(values = cols, guide = guide)
  }
}

# 5. Internal helper: plot a single segmentation level -----
.plot_single_level <- function(
  segments,
  meta,
  level,
  layout,
  edge_matrix,
  node_size,
  node_color,
  node_alpha,
  edge_width,
  edge_color,
  edge_alpha,
  show_labels,
  label_size,
  show_segments,
  segment_alpha,
  color_palette,
  theme_style,
  title,
  segment_naming,
  segment_colors,
  node_shape = "none",
  ...
) {
  # Create igraph object with validation
  g <- moneca_graph_from_adjacency(
    edge_matrix,
    mode = "directed",
    weighted = TRUE
  )

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
      mobility_matrix[, nrow(mobility_matrix)]) /
      2
    node_totals <- node_totals[-length(node_totals)]
    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(node_size = node_totals)
  } else if (identical(node_size, "mobility")) {
    # Calculate mobility rates (off-diagonal mobility)
    mobility_rates <- 1 -
      diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) /
        rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(node_size = mobility_rates)
  } else if (is.numeric(node_size) && length(node_size) == n_nodes) {
    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(node_size = node_size)
  } else {
    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(node_size = 5)
  }

  # Add segment membership with enhanced names
  if (identical(node_color, "segment")) {
    # Handle different types of segment_naming input
    if (is.data.frame(segment_naming)) {
      # Custom dataframe provided - validate structure
      if (!all(c("name", "segment_label") %in% colnames(segment_naming))) {
        stop(
          "segment_naming dataframe must have columns 'name' and 'segment_label'"
        )
      }

      # Get metadata-based membership then override labels
      membership <- .membership_from_metadata(meta, level)

      # Override segment_label with custom labels where available
      custom_match <- match(membership$name, segment_naming$name)
      custom_labels <- segment_naming$segment_label[custom_match]

      membership$segment_label <- ifelse(
        is.na(custom_labels),
        membership$segment_label,
        custom_labels
      )
    } else {
      # Handle character string or NULL input
      naming_strategy <- if (is.null(segment_naming)) "auto" else segment_naming

      # Validate character string input
      if (
        !is.character(naming_strategy) ||
          !naming_strategy %in% c("auto", "concat", "pattern", "custom")
      ) {
        stop(
          "segment_naming must be 'auto', 'concat', 'pattern', 'custom', a dataframe, or NULL"
        )
      }

      if (naming_strategy == "auto") {
        # Fast path: use pre-computed metadata
        membership <- .membership_from_metadata(meta, level)
      } else {
        # Legacy path for concat/pattern/custom strategies
        membership <- segment.membership.enhanced(
          segments,
          level = level,
          naming_strategy = naming_strategy
        )
      }
    }

    # Match node names to membership
    node_segments <- membership$membership[match(node_names, membership$name)]
    node_segments[is.na(node_segments)] <- "Unassigned"

    # Get enhanced segment labels for better legends
    node_segment_labels <- membership$segment_label[match(
      node_names,
      membership$name
    )]
    node_segment_labels[is.na(node_segment_labels)] <- "Unassigned"

    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(
        segment = as.factor(node_segments),
        segment_label = as.factor(node_segment_labels)
      )
  } else if (identical(node_color, "mobility")) {
    mobility_rates <- 1 -
      diag(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)]) /
        rowSums(mobility_matrix[-nrow(mobility_matrix), -ncol(mobility_matrix)])
    tidy_graph <- tidy_graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::mutate(mobility_rate = mobility_rates)
  }

  # Create consistent layout first that will be used for BOTH plotting and hulls
  if (is.character(layout)) {
    # Validate that the graph has nodes
    if (length(V(g)) == 0) {
      stop("Cannot create plot: graph has no nodes")
    }
    # Create the layout once and reuse it consistently
    actual_layout <- ggraph::create_layout(tidy_graph, layout = layout, ...)
    p <- ggraph::ggraph(actual_layout)
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
      warning(
        "Layout contains infinite coordinates, replacing with finite values"
      )
      x_coords[is.infinite(x_coords)] <- 0
      y_coords[is.infinite(y_coords)] <- 0
    }
    if (
      length(unique(x_coords[!is.na(x_coords)])) == 1 &&
        length(unique(y_coords[!is.na(y_coords)])) == 1
    ) {
      warning("All layout coordinates are the same, adding small random jitter")
      x_coords <- x_coords + runif(length(x_coords), -0.1, 0.1)
      y_coords <- y_coords + runif(length(y_coords), -0.1, 0.1)
    }

    # Create manual layout object with all necessary attributes
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
    p <- ggraph::ggraph(
      tidy_graph,
      layout = "manual",
      x = x_coords,
      y = y_coords
    )
  }

  # Add segment boundaries (convex hulls) FIRST as background layer
  if (show_segments && identical(node_color, "segment")) {
    # Use the SAME layout coordinates that the plot is using

    # Add segment information to layout if not already present
    if (!"segment" %in% colnames(actual_layout)) {
      actual_layout$segment <- tidy_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::pull(segment) %>%
        as.character()
    }

    if (!"segment_label" %in% colnames(actual_layout)) {
      actual_layout$segment_label <- tidy_graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::pull(segment_label) %>%
        as.character()
    }

    # Create hull data
    hull_data <- actual_layout %>%
      dplyr::group_by(segment) %>%
      dplyr::filter(dplyr::n() >= 3) %>%
      dplyr::do({
        if (nrow(.) >= 3) {
          hull_indices <- grDevices::chull(.$x, .$y)
          .[hull_indices, c("x", "y", "segment", "segment_label")]
        } else {
          data.frame(
            x = numeric(0),
            y = numeric(0),
            segment = character(0),
            segment_label = character(0)
          )
        }
      }) %>%
      dplyr::ungroup()

    if (nrow(hull_data) > 0) {
      # Calculate label positions (centroid)
      label_data <- hull_data %>%
        dplyr::group_by(segment) %>%
        dplyr::summarise(
          x = mean(x, na.rm = TRUE),
          y = mean(y, na.rm = TRUE),
          label = dplyr::first(segment_label),
          .groups = "drop"
        )

      # Add hulls as the FIRST layer (background)
      hull_list <- split(hull_data, hull_data$segment)

      for (i in seq_along(hull_list)) {
        hull_segment <- hull_list[[i]]
        if (nrow(hull_segment) >= 3) {
          # Determine fill color from the unified segment_colors map
          seg_label <- hull_segment$segment_label[1]
          fill_color <- if (
            !is.null(segment_colors) && seg_label %in% names(segment_colors)
          ) {
            segment_colors[seg_label]
          } else {
            "grey70"
          }

          if (requireNamespace("ggforce", quietly = TRUE)) {
            p <- p +
              ggforce::geom_shape(
                data = hull_segment,
                ggplot2::aes(x = x, y = y),
                fill = fill_color,
                alpha = segment_alpha,
                expand = ggplot2::unit(0.15, "cm"),
                radius = ggplot2::unit(0.25, "cm"),
                show.legend = FALSE,
                inherit.aes = FALSE
              )
          } else {
            p <- p +
              ggplot2::geom_polygon(
                data = hull_segment,
                ggplot2::aes(x = x, y = y),
                fill = fill_color,
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
    p <- p +
      ggraph::geom_edge_link(
        ggplot2::aes(width = weight, alpha = weight),
        color = edge_color
      ) +
      ggraph::scale_edge_width_continuous(range = c(0.2, 2), guide = "none") +
      ggraph::scale_edge_alpha_continuous(range = c(0.2, 0.9), guide = "none")
  } else {
    edge_width_val <- if (is.numeric(edge_width)) edge_width else 0.5
    p <- p +
      ggraph::geom_edge_link(
        color = edge_color,
        alpha = edge_alpha,
        width = edge_width_val
      )
  }

  # Add nodes (third layer - on top)
  use_shape <- identical(node_shape, "segment") &&
    identical(node_color, "segment")

  if (identical(node_color, "segment")) {
    if (use_shape) {
      p <- p +
        ggraph::geom_node_point(
          ggplot2::aes(
            size = node_size,
            color = segment_label,
            shape = segment_label
          ),
          alpha = node_alpha,
          show.legend = FALSE
        )
      # Shape scale: recycle 8 shapes for >8 segments
      shape_values <- c(16, 17, 15, 18, 8, 3, 4, 7)
      seg_labels <- sort(unique(
        tidygraph::as_tibble(tidy_graph, what = "nodes")$segment_label
      ))
      n_seg <- length(seg_labels)
      shapes <- setNames(
        rep_len(shape_values, n_seg),
        seg_labels
      )
      p <- p + ggplot2::scale_shape_manual(values = shapes, guide = "none")
    } else {
      p <- p +
        ggraph::geom_node_point(
          ggplot2::aes(size = node_size, color = segment_label),
          alpha = node_alpha,
          show.legend = FALSE
        )
    }

    # Use unified segment_colors if available, otherwise fall back
    if (!is.null(segment_colors)) {
      p <- p +
        ggplot2::scale_color_manual(values = segment_colors, guide = "none")
    } else {
      n_labels <- length(unique(
        tidygraph::as_tibble(tidy_graph, what = "nodes")$segment_label
      ))
      p <- .apply_discrete_color_scale(p, color_palette, n_labels)
    }
  } else if (identical(node_color, "mobility")) {
    p <- p +
      ggraph::geom_node_point(
        ggplot2::aes(size = node_size, color = mobility_rate),
        alpha = node_alpha,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_viridis_c(option = "viridis", guide = "none")
  } else {
    p <- p +
      ggraph::geom_node_point(
        ggplot2::aes(size = node_size),
        color = node_color,
        alpha = node_alpha,
        show.legend = FALSE
      )
  }

  # Add node labels - always show when show_labels = TRUE
  if (show_labels) {
    p <- p +
      ggraph::geom_node_text(
        ggplot2::aes(label = name),
        size = label_size,
        repel = TRUE
      )
  }

  # Add segment labels if we have hull data
  if (!is.null(hull_label_data)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p +
        ggrepel::geom_text_repel(
          data = hull_label_data,
          ggplot2::aes(x = x, y = y, label = label),
          size = label_size + 1,
          fontface = "bold",
          color = "grey30",
          inherit.aes = FALSE,
          box.padding = 0.5,
          point.padding = 0.3,
          max.overlaps = Inf
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = hull_label_data,
          ggplot2::aes(x = x, y = y, label = label),
          size = label_size + 1,
          fontface = "bold",
          color = "grey30",
          vjust = 0.5,
          inherit.aes = FALSE
        )
    }
  }

  # Apply theme with tighter margins
  if (theme_style == "void") {
    p <- p +
      ggraph::theme_graph(base_family = "") +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 11,
          margin = ggplot2::margin(b = 2)
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2)
      )
  } else if (theme_style == "minimal") {
    p <- p +
      ggplot2::theme_minimal(base_family = "") +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 11,
          margin = ggplot2::margin(b = 2)
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2)
      )
  } else if (theme_style == "classic") {
    p <- p +
      ggplot2::theme_classic(base_family = "") +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(
          size = 11,
          margin = ggplot2::margin(b = 2)
        ),
        plot.margin = ggplot2::margin(2, 2, 2, 2)
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
#' @param show_labels Logical indicating whether to display individual node labels.
#'   Default is FALSE (only segment/cluster labels are shown).
#' @param label_size Numeric size for node labels. Default is 3.
#' @param show_segments Logical indicating whether to highlight segment boundaries.
#'   Default is TRUE.
#' @param segment_alpha Numeric value (0-1) for segment boundary transparency.
#'   Default is 0.3.
#' @param color_palette Character string specifying the color palette for segments:
#'   \itemize{
#'     \item \code{"okabe-ito"} (default): 8-color CVD-safe Okabe-Ito palette.
#'       For >8 segments, falls back to viridis.
#'     \item \code{"viridis"}, \code{"plasma"}, \code{"inferno"}, \code{"cividis"},
#'       \code{"magma"}: Perceptually uniform viridis palettes (requires viridis package).
#'     \item Any RColorBrewer qualitative palette name (e.g., \code{"Set3"}, \code{"Paired"}).
#'   }
#' @param theme_style Character string specifying the plot theme:
#'   \itemize{
#'     \item "void" (default): Clean background with no axes
#'     \item "minimal": Minimal theme with subtle gridlines
#'     \item "classic": Traditional ggplot2 theme
#'   }
#' @param title Character string for plot title. Default is NULL (no title).
#' @param segment_naming Specifies how to name segments in the visualization. Can be:
#'   \itemize{
#'     \item Character string: "auto" (default), "concat", "pattern", or "custom" -
#'       these are passed to \code{\link{segment.membership.enhanced}} for automatic naming
#'     \item data.frame: Custom segment labels with columns "name" (node names from the
#'       mobility matrix) and "segment_label" (desired custom labels). This allows complete
#'       control over segment naming
#'     \item NULL: Uses default "auto" strategy
#'   }
#'   When a data.frame is provided, custom labels override automatically generated names.
#'   The data.frame approach is useful for meaningful business names (e.g., "Upper Management"
#'   instead of "Segment 1") or multilingual applications.
#' @param node_shape Controls shape encoding for nodes:
#'   \itemize{
#'     \item \code{"none"} (default): All nodes use circles (shape 16).
#'     \item \code{"segment"}: Nodes mapped to different shapes per segment,
#'       providing redundant encoding alongside color for CVD accessibility.
#'       Shapes cycle through: circle, triangle, square, diamond, asterisk, plus,
#'       cross, and square-cross.
#'   }
#' @param ... Additional arguments passed to ggraph layout functions.
#'
#' @return When \code{level} is a single integer, a \code{ggplot2} object that
#'   can be further customized. When \code{level} is a vector (including the
#'   default), a named list of \code{ggplot2} objects (one per level), each
#'   viewable individually at full size. Names are \code{"level_2"},
#'   \code{"level_3"}, etc.
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
#' \strong{Segment Naming}: The new data.frame approach for segment_naming provides
#' complete control over how segments are labeled. This is particularly useful for:
#' \itemize{
#'   \item Business applications where meaningful names are essential (e.g., "Senior Management" vs "Segment 1")
#'   \item Multilingual visualizations where labels need translation
#'   \item Presentations where consistent, professional terminology is required
#'   \item Partial customization where only specific segments need custom names
#' }
#'
#' @examples
#' \dontrun{
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
#'   color_palette = "Spectral",
#'   title = "Social Mobility Network"
#' )
#' }
#'
#' @seealso
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_ego_ggraph}} for ego network visualization,
#' \code{\link{plot_stair_ggraph}} for multi-level visualization,
#' \code{\link{segment.edges}} for edge matrix generation
#'
#' @export
plot_moneca_ggraph <- function(
  segments,
  level = NULL,
  layout = "fr",
  edges = "auto",
  node_size = "total",
  node_color = "segment",
  node_alpha = 0.8,
  edge_width = "weight",
  edge_color = "grey50",
  edge_alpha = 0.6,
  show_labels = FALSE,
  label_size = 3,
  show_segments = TRUE,
  segment_alpha = 0.3,
  color_palette = "okabe-ito",
  theme_style = "void",
  title = NULL,
  segment_naming = "auto",
  node_shape = "none",
  ...
) {
  # 1. Package checks
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required for this function. Please install it.")
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop(
      "Package 'tidygraph' is required for this function. Please install it."
    )
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for this function. Please install it.")
  }

  # 2. Validate segments object
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop(
      "segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function."
    )
  }
  if (is.null(segments$mat.list[[1]])) {
    stop(
      "segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function."
    )
  }

  # 3. Canonical metadata
  meta <- .get_metadata(segments)

  # 4. Set default level
  if (is.null(level)) {
    level <- seq_len(meta$n_levels)[-1]
  }

  # 5. Compute edge matrix once
  if (identical(edges, "auto")) {
    edge_matrix <- segment.edges(segments, level = 1)
  } else {
    edge_matrix <- edges
  }

  # 6. Validate edge matrix
  if (nrow(edge_matrix) == 0 || ncol(edge_matrix) == 0) {
    stop("Edge matrix is empty - cannot create network plot")
  }
  if (all(edge_matrix == 0, na.rm = TRUE)) {
    warning("Edge matrix contains no non-zero values - plot may be empty")
  }

  # 7. Compute unified color palette once
  if (show_segments && identical(node_color, "segment")) {
    segment_colors <- .compute_segment_colors(meta, level, color_palette)
  } else {
    segment_colors <- NULL
  }

  # 8. Single level: return ggplot directly
  if (length(level) == 1L) {
    return(.plot_single_level(
      segments = segments,
      meta = meta,
      level = level,
      layout = layout,
      edge_matrix = edge_matrix,
      node_size = node_size,
      node_color = node_color,
      node_alpha = node_alpha,
      edge_width = edge_width,
      edge_color = edge_color,
      edge_alpha = edge_alpha,
      show_labels = show_labels,
      label_size = label_size,
      show_segments = show_segments,
      segment_alpha = segment_alpha,
      color_palette = color_palette,
      theme_style = theme_style,
      title = title,
      segment_naming = segment_naming,
      segment_colors = segment_colors,
      node_shape = node_shape,
      ...
    ))
  }

  # 9. Multi-level: compute shared layout for consistent node positions
  if (is.character(layout)) {
    g_tmp <- moneca_graph_from_adjacency(
      edge_matrix,
      mode = "directed",
      weighted = TRUE
    )
    tidy_tmp <- tidygraph::as_tbl_graph(g_tmp)
    layout_obj <- ggraph::create_layout(tidy_tmp, layout = layout, ...)
    shared_layout <- as.matrix(layout_obj[, c("x", "y")])
  } else {
    shared_layout <- layout
  }

  # 10. Create one plot per level, return as named list
  plots <- lapply(level, function(lvl) {
    .plot_single_level(
      segments = segments,
      meta = meta,
      level = lvl,
      layout = shared_layout,
      edge_matrix = edge_matrix,
      node_size = node_size,
      node_color = node_color,
      node_alpha = node_alpha,
      edge_width = edge_width,
      edge_color = edge_color,
      edge_alpha = edge_alpha,
      show_labels = show_labels,
      label_size = label_size,
      show_segments = show_segments,
      segment_alpha = segment_alpha,
      color_palette = color_palette,
      theme_style = theme_style,
      title = paste("Level", lvl),
      segment_naming = segment_naming,
      segment_colors = segment_colors,
      node_shape = node_shape,
      ...
    )
  })
  names(plots) <- paste0("level_", level)
  plots
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
#' @param highlight_color Color for the ego (focal) node. Default is
#'   \code{"#D55E00"} (CVD-safe vermillion from the Okabe-Ito palette).
#' @param flow_color Character string specifying the color scheme for mobility flows.
#'   Default is "viridis". Can be any viridis variant ("viridis", "plasma", "inferno", etc.).
#' @param node_size_range Numeric vector of length 2 specifying the range for node sizes.
#'   Default is c(3, 12).
#' @param edge_width_range Numeric vector of length 2 specifying the range for edge widths.
#'   Default is c(0.5, 3).
#' @param title Character string for plot title. Default is NULL.
#' @param segment_naming Specifies how to name segments in the visualization. Can be:
#'   \itemize{
#'     \item Character string: "auto" (default), "concat", "pattern", or "custom" -
#'       these are passed to \code{\link{segment.membership.enhanced}} for automatic naming
#'     \item data.frame: Custom segment labels with columns "name" (node names from the
#'       mobility matrix) and "segment_label" (desired custom labels). This allows complete
#'       control over segment naming
#'     \item NULL: Uses default "auto" strategy
#'   }
#'   When a data.frame is provided, custom labels override automatically generated names.
#'   The data.frame approach is useful for meaningful business names or multilingual applications.
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
#' \dontrun{
#' # Generate synthetic data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#'
#' # Ego network for the middle category (index 3)
#' plot_ego_ggraph(seg, mobility_data, ego_id = 3,
#'                 title = "Mobility from Middle Class")
#' }
#'
#' @seealso
#' \code{\link{plot_moneca_ggraph}} for full network visualization,
#' \code{\link{plot_stair_ggraph}} for multi-level visualization,
#' \code{\link{moneca}} for the main analysis function
#'
#' @export
plot_ego_ggraph <- function(
  segments,
  mobility_matrix,
  ego_id,
  min_weight = 0,
  layout = "stress",
  highlight_color = "#D55E00",
  flow_color = "viridis",
  node_size_range = c(2, 8),
  edge_width_range = c(0.2, 3),
  title = NULL,
  segment_naming = "auto",
  ...
) {
  # Load required packages
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required for this function. Please install it.")
  }
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    stop(
      "Package 'tidygraph' is required for this function. Please install it."
    )
  }

  # Canonical metadata (cached or computed once)
  meta <- .get_metadata(segments)

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
  ego_matrix[ego_id, ] <- core_matrix[ego_id, ] # Outflows
  ego_matrix[, ego_id] <- core_matrix[, ego_id] # Inflows

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
  g <- moneca_graph_from_adjacency(
    filtered_matrix,
    mode = "directed",
    weighted = TRUE
  )

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
  # Handle different types of segment_naming input
  all_levels <- seq_len(meta$n_levels)

  if (is.data.frame(segment_naming)) {
    # Custom dataframe provided - validate structure
    if (!all(c("name", "segment_label") %in% colnames(segment_naming))) {
      stop(
        "segment_naming dataframe must have columns 'name' and 'segment_label'"
      )
    }

    # Get metadata-based membership then override labels
    membership <- .membership_from_metadata(meta, all_levels)

    custom_match <- match(membership$name, segment_naming$name)
    custom_labels <- segment_naming$segment_label[custom_match]

    membership$segment_label <- ifelse(
      is.na(custom_labels),
      membership$segment_label,
      custom_labels
    )
  } else {
    # Handle character string or NULL input
    naming_strategy <- if (is.null(segment_naming)) "auto" else segment_naming

    # Validate character string input
    if (
      !is.character(naming_strategy) ||
        !naming_strategy %in% c("auto", "concat", "pattern", "custom")
    ) {
      stop(
        "segment_naming must be 'auto', 'concat', 'pattern', 'custom', a dataframe, or NULL"
      )
    }

    if (naming_strategy == "auto") {
      # Fast path: use pre-computed metadata
      membership <- .membership_from_metadata(meta, all_levels)
    } else {
      # Legacy path for concat/pattern/custom strategies
      membership <- segment.membership.enhanced(
        segments,
        level = all_levels,
        naming_strategy = naming_strategy
      )
    }
  }

  # Find ego's segment
  ego_segment <- membership$membership[ego_id]
  ego_segment_label <- membership$segment_label[ego_id]

  # Determine which nodes belong to the same segment as ego
  same_segment_as_ego <- membership$membership[connected_nodes] == ego_segment

  # Get segment labels for connected nodes
  connected_segment_labels <- membership$segment_label[connected_nodes]

  ego_network <- ego_network %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(
      node_size = node_totals,
      is_ego = 1:tidygraph::graph_order() == ego_position_in_filtered,
      same_segment = same_segment_as_ego,
      node_name = node_names,
      segment_label = connected_segment_labels
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
  p <- p +
    ggraph::geom_edge_link(
      ggplot2::aes(width = weight, color = weight, alpha = weight),
      end_cap = ggraph::circle(0.05, "cm")
    ) +
    ggraph::scale_edge_width_continuous(
      range = edge_width_range,
      guide = "none"
    ) +
    ggraph::scale_edge_color_viridis(name = "Flow Volume", guide = "none") +
    ggraph::scale_edge_alpha_continuous(range = c(0.3, 0.9), guide = "none")

  # Add nodes with coloring based on segment membership
  p <- p +
    ggraph::geom_node_point(
      ggplot2::aes(size = node_size, color = interaction(is_ego, same_segment)),
      alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "FALSE.FALSE" = "#56B4E9",
        "FALSE.TRUE" = highlight_color,
        "TRUE.FALSE" = highlight_color,
        "TRUE.TRUE" = highlight_color
      ),
      guide = "none"
    ) +
    ggplot2::scale_size_continuous(
      range = node_size_range,
      name = "Total Mobility"
    )

  # Add labels - use individual node names (professions)
  p <- p +
    ggraph::geom_node_text(
      ggplot2::aes(label = node_name),
      size = 3,
      repel = TRUE
    )

  # Apply theme with reduced title spacing
  p <- p +
    ggraph::theme_graph() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = 11,
        margin = ggplot2::margin(b = 2)
      )
    )

  # Add title
  if (is.null(title)) {
    ego_name <- node_names[ego_position_in_filtered]
    if (min_weight > 0) {
      title <- paste(
        "Mobility Network for",
        ego_name,
        paste0("(min weight: ", min_weight, ")")
      )
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
#' @param segment_naming Specifies how to name segments across all visualization levels. Can be:
#'   \itemize{
#'     \item Character string: "auto" (default), "concat", "pattern", or "custom" -
#'       these are passed to \code{\link{segment.membership.enhanced}} for automatic naming
#'     \item data.frame: Custom segment labels with columns "name" (node names from the
#'       mobility matrix) and "segment_label" (desired custom labels). This allows complete
#'       control over segment naming across all hierarchical levels
#'     \item NULL: Uses default "auto" strategy
#'   }
#'   When a data.frame is provided, custom labels override automatically generated names
#'   consistently across all levels. The data.frame approach is particularly useful for
#'   stair plots as it maintains consistent naming across the hierarchical progression.
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
#' \dontrun{
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
#'                                  ncol = 1)
#' }
#'
#' @seealso
#' \code{\link{plot_moneca_ggraph}} for single-level visualization,
#' \code{\link{plot_ego_ggraph}} for ego network analysis,
#' \code{\link{layout.matrix}} for consistent layouts,
#' \code{\link{moneca}} for the main analysis function
#'
#' @export
plot_stair_ggraph <- function(
  segments,
  levels = seq_along(segments$segment.list),
  layout = NULL,
  ncol = 2,
  segment_naming = "auto",
  include_first_level = TRUE,
  ...
) {
  # Validate segments object
  if (is.null(segments)) {
    stop("segments object is NULL")
  }
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }
  if (is.null(segments$mat.list) || length(segments$mat.list) == 0) {
    stop(
      "segments$mat.list is empty - the moneca object appears to be incomplete. Please re-run the moneca() function."
    )
  }
  if (is.null(segments$mat.list[[1]])) {
    stop(
      "segments$mat.list[[1]] is NULL - the moneca object appears to be incomplete. Please re-run the moneca() function."
    )
  }

  # Canonical metadata (cached or computed once)
  meta <- .get_metadata(segments)

  # Create consistent layout if not provided
  if (is.null(layout)) {
    # Use the layout.matrix function from MONECA
    layout <- layout.matrix(segments)
  }

  plots <- list()

  # Add first level plot (individual nodes) if requested
  if (include_first_level) {
    # Extract arguments from ... and handle show_segments specially
    dots <- list(...)
    # Remove show_segments from dots since we want to control it for level 1
    dots$show_segments <- NULL

    if ("title" %in% names(dots)) {
      # User provided a title - don't override it
      p_first <- do.call(
        plot_moneca_ggraph,
        c(
          list(
            segments = segments,
            level = 1,
            layout = layout,
            segment_naming = segment_naming,
            show_segments = FALSE, # No hulls for individual level
            show_labels = TRUE
          ),
          dots
        )
      )
    } else {
      # Use default title
      p_first <- do.call(
        plot_moneca_ggraph,
        c(
          list(
            segments = segments,
            level = 1,
            layout = layout,
            segment_naming = segment_naming,
            show_segments = FALSE, # No hulls for individual level
            show_labels = TRUE,
            title = "Level 1: Individual Classes"
          ),
          dots
        )
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

    # Extract arguments from ... (for subsequent levels, we preserve user's show_segments preference)
    dots <- list(...)

    if ("title" %in% names(dots)) {
      # User provided a title - don't override it
      p <- do.call(
        plot_moneca_ggraph,
        c(
          list(
            segments = segments,
            level = level_idx,
            layout = layout,
            segment_naming = segment_naming
          ),
          dots
        )
      )
    } else {
      # Use default title
      p <- do.call(
        plot_moneca_ggraph,
        c(
          list(
            segments = segments,
            level = level_idx,
            layout = layout,
            segment_naming = segment_naming,
            title = paste("Level", level_idx, "Segmentation")
          ),
          dots
        )
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

# ==============================================================================
# Private helpers for plot_moneca_dendrogram (elbow style)
# ==============================================================================

# .dend_parent_map -----
# Returns a list `parent_of` of length top_level - 1.
# parent_of[[k]] is an integer vector of length
# length(meta$levels[[k]]$groups) where element j is the segment_id at level
# k+1 that contains group j, or NA if the group has no parent in [1, top_level].
.dend_parent_map <- function(meta, top_level) {
  parent_of <- vector("list", top_level - 1L)
  for (k in seq_len(top_level - 1L)) {
    child_groups <- meta$levels[[k]]$groups
    parent_groups <- meta$levels[[k + 1L]]$groups
    parent_of[[k]] <- vapply(
      child_groups,
      function(g) {
        idx <- which(vapply(
          parent_groups,
          function(parent) length(g) > 0L && all(g %in% parent),
          logical(1L)
        ))
        if (length(idx) == 0L) NA_integer_ else idx[[1L]]
      },
      integer(1L)
    )
  }
  parent_of
}

# .dend_leaf_order -----
# Recursive DFS from top_level down to level 1. Returns an integer vector of
# base-node (level-1) indices in their left-to-right display order.
.dend_leaf_order <- function(meta, top_level, parent_of) {
  .recurse <- function(k, seg_id) {
    if (k == 1L) {
      # Base case: the group members are the leaf indices
      return(meta$levels[[1L]]$groups[[seg_id]])
    }
    children <- which(parent_of[[k - 1L]] == seg_id)
    if (length(children) == 0L) {
      # No children recorded; fall back to raw members
      return(meta$levels[[k]]$groups[[seg_id]])
    }
    unlist(
      lapply(children, function(child) .recurse(k - 1L, child)),
      use.names = FALSE
    )
  }

  n_top <- length(meta$levels[[top_level]]$groups)
  order_vec <- unlist(
    lapply(seq_len(n_top), function(s) .recurse(top_level, s)),
    use.names = FALSE
  )
  order_vec
}

# .dend_node_positions -----
# Returns a data.frame(level, segment_id, x) where leaves get their given
# leaf_x and internal segments get the mean x of their leaf descendants.
# leaf_x: named numeric vector, names = base-node original names (or indices).
.dend_node_positions <- function(meta, top_level, leaf_x) {
  rows <- vector("list", top_level)

  # Level 1: leaves
  n_leaves <- length(leaf_x)
  rows[[1L]] <- data.frame(
    level = 1L,
    segment_id = seq_len(n_leaves),
    x = as.numeric(leaf_x),
    stringsAsFactors = FALSE
  )

  # Levels 2..top_level: mean of leaf descendants
  for (k in 2L:top_level) {
    grps <- meta$levels[[k]]$groups
    x_vals <- vapply(
      grps,
      function(g) {
        mean(leaf_x[g])
      },
      numeric(1L)
    )
    rows[[k]] <- data.frame(
      level = k,
      segment_id = seq_along(grps),
      x = x_vals,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# .dend_heights -----
# Returns a numeric vector of length top_level giving the y-axis height of
# each level. levels[[1]] is always the lowest (leaves).
.dend_heights <- function(meta, top_level, method, segments_obj) {
  seg_list <- segments_obj$segment.list
  n_lvl <- meta$n_levels

  raw <- switch(
    method,
    "level" = seq_len(top_level),
    "uniform" = {
      h <- seq(0, top_level - 1L)
      if (length(h) > 1L && max(h) > min(h)) {
        (h - min(h)) / (max(h) - min(h))
      } else {
        h
      }
    },
    "mobility" = {
      mob_rates <- vapply(
        seq_len(n_lvl),
        function(i) {
          mat <- segments_obj$mat.list[[i]]
          l <- nrow(mat)
          if (l <= 1L) {
            return(1)
          }
          diag_sum <- sum(diag(mat)[-l])
          total_sum <- sum(mat[-l, -l])
          if (total_sum == 0) {
            return(1)
          }
          diag_sum / total_sum
        },
        numeric(1L)
      )
      h <- cumsum(c(0, diff(mob_rates)))[seq_len(top_level)]
      if (length(h) > 1L && max(h) > min(h)) {
        (h - min(h)) / (max(h) - min(h))
      } else {
        seq_len(top_level) - 1L
      }
    },
    "segments" = {
      n_segs <- vapply(seg_list, length, integer(1L))
      h <- cumsum(c(0, -diff(log(n_segs + 1))))[seq_len(top_level)]
      if (length(h) > 1L && max(h) > min(h)) {
        (h - min(h)) / (max(h) - min(h))
      } else {
        seq_len(top_level) - 1L
      }
    },
    stop(
      "Invalid height_method. Choose 'level', 'uniform', 'mobility', or 'segments'.",
      call. = FALSE
    )
  )
  raw
}

# .dend_build_edges -----
# Constructs an elbow edge data.frame from parent_of + positions + heights.
# Every row satisfies: x == xend  OR  y == yend  (pure L-shape, no diagonals).
# Columns: x, xend, y, yend, edge_color (NA placeholder), level_child,
#          segment_id_child, segment_id_parent, orientation.
.dend_build_edges <- function(parent_of, positions, heights, top_level) {
  rows <- vector("list", (top_level - 1L) * 4L) # generous pre-alloc
  ri <- 0L

  pos_lookup <- function(k, seg_id) {
    sub_df <- positions[positions$level == k & positions$segment_id == seg_id, ]
    sub_df$x[[1L]]
  }

  for (k in seq_len(top_level - 1L)) {
    parent_ids <- parent_of[[k]]
    y_child <- heights[k]
    y_parent <- heights[k + 1L]

    # Group children by parent
    unique_parents <- unique(parent_ids[!is.na(parent_ids)])
    for (p_id in unique_parents) {
      child_ids <- which(parent_ids == p_id)
      x_parent <- pos_lookup(k + 1L, p_id)
      x_children <- vapply(
        child_ids,
        function(cid) pos_lookup(k, cid),
        numeric(1L)
      )

      for (ci in seq_along(child_ids)) {
        cid <- child_ids[[ci]]
        x_c <- x_children[[ci]]
        # Vertical leg: child's x, from y_child up to y_parent
        ri <- ri + 1L
        rows[[ri]] <- data.frame(
          x = x_c,
          xend = x_c,
          y = y_child,
          yend = y_parent,
          edge_color = NA_character_,
          level_child = k,
          segment_id_child = cid,
          segment_id_parent = p_id,
          orientation = "vertical",
          stringsAsFactors = FALSE
        )
      }

      # Horizontal bar at y_parent spanning min to max x of children
      x_lo <- min(x_children)
      x_hi <- max(x_children)
      if (x_hi > x_lo) {
        ri <- ri + 1L
        rows[[ri]] <- data.frame(
          x = x_lo,
          xend = x_hi,
          y = y_parent,
          yend = y_parent,
          edge_color = NA_character_,
          level_child = k,
          segment_id_child = NA_integer_,
          segment_id_parent = p_id,
          orientation = "horizontal",
          stringsAsFactors = FALSE
        )
      }
    }
  }
  do.call(rbind, rows[seq_len(ri)])
}

# .dend_path_to_root -----
# Returns an integer vector of segment_ids from leaf (level 1) up to top_level.
# Result[k] = segment_id at level k for the path.
.dend_path_to_root <- function(parent_of, leaf_idx, top_level) {
  path <- integer(top_level)
  path[1L] <- leaf_idx
  for (k in seq_len(top_level - 1L)) {
    p <- parent_of[[k]][path[k]]
    path[k + 1L] <- if (is.na(p)) path[k] else p
  }
  path
}

# .dend_color_paths -----
# Returns a named character vector of colours, one per (level, segment_id) key.
# Key format: "L<level>_S<segment_id>".
# Each segment on the path from a leaf to its top-level ancestor gets the
# colour of its top ancestor.
.dend_color_paths <- function(
  parent_of,
  leaf_top_segment,
  top_map_labels,
  segment_colors,
  top_level
) {
  n_leaves <- length(leaf_top_segment)
  key_color <- character(0)

  for (leaf_idx in seq_len(n_leaves)) {
    top_seg <- leaf_top_segment[[leaf_idx]]
    col <- segment_colors[[top_map_labels[top_seg]]]
    if (is.null(col) || is.na(col)) {
      col <- "grey50"
    }

    path <- .dend_path_to_root(parent_of, leaf_idx, top_level)
    for (k in seq_len(top_level)) {
      key <- paste0("L", k, "_S", path[k])
      key_color[key] <- col
    }
  }
  key_color
}

# .dend_legacy_curved -----
# Preserves the original curved-branch renderer verbatim.
# Called when style = "curved" (deprecated).
.dend_legacy_curved <- function(
  segments,
  height_method,
  color_palette,
  show_labels,
  label_size,
  branch_width,
  title,
  subtitle,
  theme_style,
  vertical
) {
  meta <- .get_metadata(segments)
  seg_list <- segments$segment.list
  n_levels <- meta$n_levels
  cat_names <- meta$original_names
  n_categories <- length(cat_names)

  edges <- data.frame(
    from = character(),
    to = character(),
    x1 = numeric(),
    y1 = numeric(),
    x2 = numeric(),
    y2 = numeric(),
    level = integer(),
    segment = character(),
    stringsAsFactors = FALSE
  )

  heights <- switch(
    height_method,
    "uniform" = seq(0, n_levels - 1),
    "level" = seq(0, n_levels - 1),
    "mobility" = {
      mob_rates <- sapply(seq_len(n_levels), function(i) {
        mat <- segments$mat.list[[i]]
        l <- nrow(mat)
        if (l <= 1) {
          return(1)
        }
        diag_sum <- sum(diag(mat)[-l])
        total_sum <- sum(mat[-l, -l])
        if (total_sum == 0) {
          return(1)
        }
        diag_sum / total_sum
      })
      cumsum(c(0, diff(mob_rates)))
    },
    "segments" = {
      n_segs <- sapply(seg_list, length)
      cumsum(c(0, -diff(log(n_segs + 1))))
    },
    seq(0, n_levels - 1)
  )

  if (length(heights) > 1 && max(heights) > min(heights)) {
    heights <- (heights - min(heights)) / (max(heights) - min(heights))
  }

  node_positions <- list()

  ordered_cats <- seq_len(n_categories)
  if (n_levels > 1) {
    for (level in n_levels:2) {
      segments_at_level <- seg_list[[level]]
      if (length(segments_at_level) == 0) {
        next
      }
      new_order <- integer(0)
      for (seg_idx in seq_along(segments_at_level)) {
        segment_members <- segments_at_level[[seg_idx]]
        segment_members_ordered <- segment_members[order(match(
          segment_members,
          ordered_cats
        ))]
        new_order <- c(new_order, segment_members_ordered)
      }
      missing_cats <- setdiff(ordered_cats, new_order)
      if (length(missing_cats) > 0) {
        missing_cats_ordered <- missing_cats[order(match(
          missing_cats,
          ordered_cats
        ))]
        new_order <- c(new_order, missing_cats_ordered)
      }
      ordered_cats <- new_order
    }
  }

  node_positions[[1]] <- data.frame(
    node_id = paste0("L1_", seq_len(n_categories)),
    label = cat_names,
    x = heights[1],
    y = match(seq_len(n_categories), ordered_cats),
    level = 1,
    segment_id = seq_len(n_categories),
    original_index = seq_len(n_categories),
    stringsAsFactors = FALSE
  )

  for (level in 2:n_levels) {
    segments_at_level <- seg_list[[level]]
    n_segments <- length(segments_at_level)
    if (n_segments == 0) {
      next
    }
    y_positions <- numeric(n_segments)
    segment_labels <- character(n_segments)
    for (i in seq_len(n_segments)) {
      members <- segments_at_level[[i]]
      member_y <- node_positions[[1]]$y[members]
      y_positions[i] <- mean(member_y)
      segment_labels[i] <- meta$levels[[level]]$map$label[i]
    }
    node_positions[[level]] <- data.frame(
      node_id = paste0("L", level, "_", seq_len(n_segments)),
      label = segment_labels,
      x = heights[level],
      y = y_positions,
      level = level,
      segment_id = seq_len(n_segments),
      original_index = NA,
      stringsAsFactors = FALSE
    )
    for (i in seq_len(n_segments)) {
      members <- segments_at_level[[i]]
      if (level == 2) {
        prev_nodes <- node_positions[[1]][members, ]
      } else {
        prev_segments <- seg_list[[level - 1]]
        prev_indices <- which(sapply(prev_segments, function(ps) {
          any(members %in% ps)
        }))
        prev_nodes <- node_positions[[level - 1]][prev_indices, ]
      }
      current_node <- node_positions[[level]][i, ]
      for (j in seq_len(nrow(prev_nodes))) {
        new_edge <- data.frame(
          from = prev_nodes$node_id[j],
          to = current_node$node_id,
          x1 = prev_nodes$x[j],
          y1 = prev_nodes$y[j],
          x2 = current_node$x,
          y2 = current_node$y,
          level = level,
          segment = paste0("S", level, ".", i),
          stringsAsFactors = FALSE
        )
        edges <- rbind(edges, new_edge)
      }
    }
  }

  all_nodes <- do.call(rbind, node_positions)

  final_segments <- seg_list[[n_levels]]
  n_final_segments <- length(final_segments)
  segment_colors <- .resolve_palette_colors(n_final_segments, color_palette)

  edge_colors <- character(nrow(edges))
  for (i in seq_len(nrow(edges))) {
    edge_level <- edges$level[i]
    if (edge_level == n_levels) {
      seg_idx <- as.numeric(gsub("S\\d+\\.(\\d+)", "\\1", edges$segment[i]))
      edge_colors[i] <- segment_colors[seg_idx]
    } else {
      edge_colors[i] <- "black"
    }
  }
  edges$color <- edge_colors

  p <- ggplot2::ggplot()
  if (nrow(edges) > 0) {
    p <- p +
      ggplot2::geom_curve(
        data = edges,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2, color = color),
        linewidth = branch_width,
        curvature = 0.2,
        angle = 90,
        ncp = 5,
        lineend = "round"
      )
  }
  p <- p +
    ggplot2::geom_point(
      data = all_nodes,
      ggplot2::aes(x = x, y = y),
      size = 3,
      color = "black"
    )
  if (
    isTRUE(show_labels) ||
      identical(show_labels, "leaves") ||
      identical(show_labels, "both")
  ) {
    bottom_nodes <- all_nodes[all_nodes$level == 1, ]
    bottom_nodes <- bottom_nodes[order(bottom_nodes$y), ]
    bottom_nodes$label <- cat_names[ordered_cats]
    p <- p +
      ggplot2::geom_text(
        data = bottom_nodes,
        ggplot2::aes(x = x, y = y, label = label),
        hjust = -0.1,
        size = label_size,
        angle = if (vertical) 0 else 45,
        vjust = 0.5
      )
  }
  p <- p +
    switch(
      theme_style,
      "minimal" = ggplot2::theme_minimal(),
      "classic" = ggplot2::theme_classic(),
      "void" = ggplot2::theme_void(),
      ggplot2::theme_minimal()
    )
  p <- p +
    ggplot2::theme(
      legend.position = "none",
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5)
    )
  p <- p + ggplot2::labs(title = title, subtitle = subtitle)
  p <- p + ggplot2::scale_color_identity()
  if (vertical) {
    p <- p + ggplot2::coord_flip()
    p <- p +
      ggplot2::expand_limits(x = c(min(heights) - 0.1, max(heights) + 0.1))
  } else {
    p <- p +
      ggplot2::expand_limits(y = c(min(heights) - 0.1, max(heights) + 0.1))
  }
  p
}

# ==============================================================================
# Public function
# ==============================================================================

#' Plot MONECA Results as a Dendrogram
#'
#' @description
#' Creates a dendrogram visualization of the hierarchical clustering produced by
#' [moneca()] or [moneca_fast()]. The default `style = "elbow"` renders classic
#' L-shaped branches, making it straightforward to read: (a) each leaf's
#' horizontal position, (b) the exact level at which two nodes merge, and (c)
#' the path from any leaf up to its top-level cluster.
#'
#' The legacy curved-branch look is still reachable via `style = "curved"`, but
#' that style is deprecated and will emit a warning.
#'
#' @param segments A `moneca` object returned by [moneca()] or [moneca_fast()].
#' @param top_level Integer. The highest segmentation level to include. Defaults
#'   to `meta$n_levels`. Must be `>= 2`.
#' @param height_method Character. How to assign y-axis positions to levels.
#'   One of:
#'   \describe{
#'     \item{`"level"`}{Default. Integer level number (1, 2, ...). Makes "at
#'       level k" directly readable on the axis.}
#'     \item{`"uniform"`}{Equally spaced positions in [0, 1].}
#'     \item{`"mobility"`}{Heights derived from mobility-reduction rates.}
#'     \item{`"segments"`}{Heights derived from the log count of segments.}
#'   }
#' @param style Character. Branch style. One of `"elbow"` (default,
#'   L-shaped branches) or `"curved"` (deprecated legacy curves).
#' @param color_by Character. How to colour branches. One of:
#'   \describe{
#'     \item{`"top_segment"`}{Default. Every branch inherits the colour of its
#'       top-level ancestor cluster.}
#'     \item{`"level"`}{Each segmentation level gets a distinct colour.}
#'     \item{`"none"`}{All branches in neutral grey.}
#'   }
#' @param show_labels Character. Which text labels to render. One of:
#'   \describe{
#'     \item{`"leaves"`}{Default. Labels for base nodes only.}
#'     \item{`"both"`}{Labels for both leaves and internal join points.}
#'     \item{`"internal"`}{Labels for internal join points only.}
#'     \item{`"none"`}{No labels.}
#'   }
#' @param highlight Character or integer. Name or 1-based index of a leaf whose
#'   path to the root should be highlighted in a thicker, dark stroke. `NULL`
#'   (default) disables highlighting.
#' @param label_size_leaf Numeric. Font size for leaf labels. Default `2.8`.
#' @param label_size_internal Numeric. Font size for internal-node labels.
#'   Default `3`.
#' @param leaf_angle Numeric or `NULL`. Rotation angle for leaf labels in
#'   degrees. When `NULL` (default), automatically set to 90 if `n > 20`,
#'   45 if `n > 10`, else 0.
#' @param leaf_label_position Character. One of `"below"` (default, `geom_text`
#'   below the leaf row) or `"on_axis"` (axis tick labels instead of
#'   `geom_text`).
#' @param level_guides Logical. Whether to draw dashed horizontal guide lines
#'   at each level height. Default `TRUE`.
#' @param level_axis_labels Logical. Whether to replace numeric axis ticks with
#'   level labels (`L1`, `L2`, ...) on both sides of the y-axis. Default `TRUE`.
#' @param branch_width Numeric. `linewidth` for normal branches. Default `0.7`.
#' @param highlight_width Numeric. `linewidth` for the highlighted path.
#'   Default `1.6`.
#' @param color_palette Character. CVD-safe palette name. Default
#'   `"okabe-ito"`. Also accepts RColorBrewer names or viridis options.
#' @param theme_style Character. ggplot2 base theme. One of `"minimal"`
#'   (default), `"void"`, or `"classic"`.
#' @param title Character. Plot title. Default `NULL`.
#' @param subtitle Character. Plot subtitle. Default `NULL`.
#' @param vertical Logical. When `TRUE` (default), leaves are on the x-axis and
#'   levels grow upward. When `FALSE`, `coord_flip()` is applied.
#' @param ... Reserved for future use.
#'
#' @return A `ggplot` object.
#'
#' @details
#' ## Elbow algorithm
#' For each parent–child pair the function emits two line segments: a vertical
#' leg from the child's x-position up to the parent's y-height, and one
#' horizontal bar at the parent's y-height spanning the full x-range of its
#' children. Every edge satisfies `x == xend OR y == yend`; no diagonal lines
#' are produced.
#'
#' ## Leaf ordering
#' A DFS from the top level assigns x-positions so that every top-level
#' cluster's leaves occupy a contiguous, crossing-free x-range.
#'
#' ## Colour continuity
#' When `color_by = "top_segment"`, every branch on the path from a leaf to
#' its top ancestor is painted with the ancestor's colour, giving full visual
#' continuity from root to leaf.
#'
#' ## Large hierarchies
#' For `n > 40` nodes consider setting `leaf_angle = 90` and
#' `leaf_label_position = "on_axis"`. Beyond ~60 nodes the layout may become
#' crowded; [plot_moneca_hierarchical()] offers a circle-packing alternative.
#'
#' @examples
#' \dontrun{
#' library(moneca)
#' mob <- generate_mobility_data(n_classes = 10, seed = 42)
#' seg <- moneca(mob, segment.levels = 3)
#'
#' # Default elbow dendrogram
#' plot_moneca_dendrogram(seg)
#'
#' # Level-number y-axis, both labels, highlight one leaf
#' plot_moneca_dendrogram(
#'   seg,
#'   height_method = "level",
#'   show_labels = "both",
#'   highlight = "Class_3"
#' )
#'
#' # Horizontal orientation, coloured by level
#' plot_moneca_dendrogram(seg, vertical = FALSE, color_by = "level")
#'
#' # Legacy curved style (deprecated)
#' plot_moneca_dendrogram(seg, style = "curved")
#' }
#'
#' @seealso
#' [moneca()], [moneca_fast()], [plot_moneca_ggraph()],
#' [plot_stair_ggraph()], [plot_moneca_hierarchical()]
#'
#' @export
plot_moneca_dendrogram <- function(
  segments,
  top_level = NULL,
  height_method = c("level", "uniform", "mobility", "segments"),
  style = c("elbow", "curved"),
  color_by = c("top_segment", "level", "none"),
  show_labels = c("leaves", "both", "internal", "none"),
  highlight = NULL,
  label_size_leaf = 2.8,
  label_size_internal = 3,
  leaf_angle = NULL,
  leaf_label_position = c("below", "on_axis"),
  level_guides = TRUE,
  level_axis_labels = TRUE,
  branch_width = 0.7,
  highlight_width = 1.6,
  color_palette = "okabe-ito",
  theme_style = c("minimal", "void", "classic"),
  title = NULL,
  subtitle = NULL,
  vertical = TRUE,
  ...
) {
  # 1. Validate & coerce enums -----
  if (!inherits(segments, "moneca")) {
    stop(
      "segments must be a moneca object created by the moneca() function.",
      call. = FALSE
    )
  }
  height_method <- match.arg(height_method)
  style <- match.arg(style)
  color_by <- match.arg(color_by)
  show_labels <- match.arg(show_labels)
  leaf_label_position <- match.arg(leaf_label_position)
  theme_style <- match.arg(theme_style)

  # 2. Deprecated curved path -----
  if (style == "curved") {
    .Deprecated(
      msg = paste0(
        "style = 'curved' is deprecated; use the new default style = 'elbow'."
      ),
      old = "style = 'curved'"
    )
    # Map legacy booleans that callers may have passed
    show_lbl <- !identical(show_labels, "none")
    lbl_size <- label_size_leaf
    return(.dend_legacy_curved(
      segments = segments,
      height_method = height_method,
      color_palette = color_palette,
      show_labels = show_lbl,
      label_size = lbl_size,
      branch_width = branch_width,
      title = title,
      subtitle = subtitle,
      theme_style = theme_style,
      vertical = vertical
    ))
  }

  # 3. Metadata -----
  meta <- .get_metadata(segments)

  if (is.null(top_level)) {
    top_level <- meta$n_levels
  }
  top_level <- as.integer(top_level)
  if (top_level < 2L) {
    stop(
      "Dendrogram requires at least 2 levels; this segments object has only 1.",
      call. = FALSE
    )
  }
  top_level <- min(top_level, meta$n_levels)

  cat_names <- meta$original_names
  n_leaves <- length(cat_names)

  # 4. Auto leaf_angle -----
  if (is.null(leaf_angle)) {
    leaf_angle <- if (n_leaves > 20L) {
      90
    } else if (n_leaves > 10L) {
      45
    } else {
      0
    }
  }

  # 5. Tree topology helpers -----
  parent_of <- .dend_parent_map(meta, top_level)
  leaf_order <- .dend_leaf_order(meta, top_level, parent_of)

  # leaf_x: leaf_order[i] = original index of the leaf at position i
  leaf_x <- numeric(n_leaves)
  for (pos in seq_len(n_leaves)) {
    leaf_x[leaf_order[pos]] <- pos
  }

  positions <- .dend_node_positions(meta, top_level, leaf_x)
  heights <- .dend_heights(meta, top_level, height_method, segments)

  # 6. Colour map -----
  top_groups <- meta$levels[[top_level]]$groups
  top_map_labels <- meta$levels[[top_level]]$map$label
  n_top <- length(top_groups)

  segment_colors <- setNames(
    .resolve_palette_colors(n_top, color_palette),
    top_map_labels
  )

  # For each leaf, find its top-level segment id
  leaf_top_seg <- integer(n_leaves)
  for (seg_id in seq_len(n_top)) {
    for (idx in top_groups[[seg_id]]) {
      leaf_top_seg[idx] <- seg_id
    }
  }

  key_colors <- .dend_color_paths(
    parent_of = parent_of,
    leaf_top_segment = leaf_top_seg,
    top_map_labels = top_map_labels,
    segment_colors = segment_colors,
    top_level = top_level
  )

  # 7. Build elbow edges -----
  edge_df <- .dend_build_edges(parent_of, positions, heights, top_level)

  if (!is.null(edge_df) && nrow(edge_df) > 0L) {
    # Assign colours per edge
    edge_df$edge_color <- vapply(
      seq_len(nrow(edge_df)),
      function(i) {
        row_i <- edge_df[i, ]
        if (color_by == "none") {
          return("grey50")
        }
        if (color_by == "level") {
          # Colour by the child level
          lvl_palette <- .resolve_palette_colors(top_level, color_palette)
          return(lvl_palette[row_i$level_child])
        }
        # color_by == "top_segment"
        if (!is.na(row_i$segment_id_child)) {
          key <- paste0("L", row_i$level_child, "_S", row_i$segment_id_child)
        } else {
          # Horizontal bar: use parent's colour via any of its children
          key <- paste0(
            "L",
            row_i$level_child + 1L,
            "_S",
            row_i$segment_id_parent
          )
        }
        col <- key_colors[key]
        if (is.null(col) || is.na(col)) "grey50" else col
      },
      character(1L)
    )
  }

  # 8. Highlight path -----
  highlight_edge_df <- NULL
  if (!is.null(highlight)) {
    hl_idx <- if (is.character(highlight)) {
      match(highlight, cat_names)
    } else {
      as.integer(highlight)
    }
    if (!is.na(hl_idx) && hl_idx >= 1L && hl_idx <= n_leaves) {
      hl_path <- .dend_path_to_root(parent_of, hl_idx, top_level)
      # Filter edge_df for edges on this path
      if (!is.null(edge_df) && nrow(edge_df) > 0L) {
        hl_mask <- vapply(
          seq_len(nrow(edge_df)),
          function(i) {
            row_i <- edge_df[i, ]
            k <- row_i$level_child
            cid <- row_i$segment_id_child
            pid <- row_i$segment_id_parent
            if (!is.na(cid)) {
              cid == hl_path[k]
            } else {
              pid == hl_path[k + 1L]
            }
          },
          logical(1L)
        )
        highlight_edge_df <- edge_df[hl_mask, ]
      }
    }
  }

  # 9. Build plot -----
  p <- ggplot2::ggplot()

  # Theme
  p <- p +
    switch(
      theme_style,
      "minimal" = ggplot2::theme_minimal(),
      "void" = ggplot2::theme_void(),
      "classic" = ggplot2::theme_classic()
    )

  # Level guide lines
  if (level_guides) {
    guide_df <- data.frame(y = heights, stringsAsFactors = FALSE)
    p <- p +
      ggplot2::geom_hline(
        data = guide_df,
        ggplot2::aes(yintercept = y),
        linetype = "dashed",
        alpha = 0.2,
        colour = "grey50"
      )
  }

  # Elbow edges
  if (!is.null(edge_df) && nrow(edge_df) > 0L) {
    p <- p +
      ggplot2::geom_segment(
        data = edge_df,
        ggplot2::aes(
          x = x,
          xend = xend,
          y = y,
          yend = yend,
          colour = edge_color
        ),
        linewidth = branch_width,
        lineend = "round"
      ) +
      ggplot2::scale_colour_identity()
  }

  # Highlight path (overplotted)
  if (!is.null(highlight_edge_df) && nrow(highlight_edge_df) > 0L) {
    p <- p +
      ggplot2::geom_segment(
        data = highlight_edge_df,
        ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        colour = "grey10",
        linewidth = highlight_width,
        lineend = "round"
      )
  }

  # Internal-node points and labels
  if (show_labels %in% c("both", "internal")) {
    internal_pos <- positions[positions$level > 1L, ]
    # Merge labels from metadata
    internal_pos$seg_label <- vapply(
      seq_len(nrow(internal_pos)),
      function(i) {
        k <- internal_pos$level[i]
        sid <- internal_pos$segment_id[i]
        meta$levels[[k]]$map$label[sid]
      },
      character(1L)
    )

    internal_pos$pt_y <- heights[internal_pos$level]

    p <- p +
      ggplot2::geom_point(
        data = internal_pos,
        ggplot2::aes(x = x, y = pt_y),
        size = 1.2,
        color = "grey20"
      )

    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p +
        ggrepel::geom_text_repel(
          data = internal_pos,
          ggplot2::aes(x = x, y = pt_y, label = seg_label),
          size = label_size_internal,
          fontface = "plain",
          min.segment.length = 0
        )
    } else {
      p <- p +
        ggplot2::geom_text(
          data = internal_pos,
          ggplot2::aes(x = x, y = pt_y, label = seg_label),
          size = label_size_internal,
          fontface = "plain",
          vjust = -0.5
        )
    }
  }

  # Leaf points
  leaf_pos <- positions[positions$level == 1L, ]
  leaf_pos$pt_y <- heights[1L]
  p <- p +
    ggplot2::geom_point(
      data = leaf_pos,
      ggplot2::aes(x = x, y = pt_y),
      size = 1.2,
      color = "grey30"
    )

  # Leaf labels
  #
  # Space allocation: rely on clip = "off" + plot.margin (below) to
  # reserve physical space for rotated labels OUTSIDE the panel. Do
  # NOT expand the data y-scale — that would dead-zone the tree into
  # a sliver at the top of the plot.
  label_margin_mm <- 0
  if (show_labels %in% c("leaves", "both")) {
    leaf_names_ordered <- cat_names[leaf_order]
    max_label_chars <- max(nchar(leaf_names_ordered), 1L)
    angle_rad <- leaf_angle * pi / 180
    # Approximate text extent: at ggplot `size = s`, one char is
    # roughly s * .pt ~= s * 2.8 pt wide (~s * 1 mm). At angle = 90
    # the full string length projects vertically. At angle = 0 only
    # a single line height does.
    char_width_mm <- label_size_leaf * 1.0
    line_height_mm <- label_size_leaf * 1.2
    projected_mm <- if (leaf_angle == 0) {
      line_height_mm
    } else {
      (max_label_chars * char_width_mm) *
        abs(sin(angle_rad)) +
        line_height_mm * abs(cos(angle_rad))
    }
    # Small safety cushion.
    label_margin_mm <- projected_mm + 3

    leaf_label_df <- data.frame(
      x = seq_len(n_leaves),
      pt_y = heights[1L],
      label = leaf_names_ordered,
      stringsAsFactors = FALSE
    )

    if (leaf_label_position == "on_axis") {
      p <- p +
        ggplot2::scale_x_continuous(
          breaks = seq_len(n_leaves),
          labels = leaf_names_ordered,
          expand = ggplot2::expansion(add = 0.5)
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = leaf_angle,
            hjust = if (leaf_angle > 0) 1 else 0.5
          )
        )
    } else {
      y_offset <- diff(range(heights)) * 0.03
      if (y_offset == 0) {
        y_offset <- 0.05
      }
      leaf_label_df$label_y <- heights[1L] - y_offset

      h_just <- if (leaf_angle == 90) {
        1
      } else if (leaf_angle == 45) {
        1
      } else {
        0.5
      }
      v_just <- if (leaf_angle == 45) 1 else 0.5

      p <- p +
        ggplot2::geom_text(
          data = leaf_label_df,
          ggplot2::aes(x = x, y = label_y, label = label),
          angle = leaf_angle,
          size = label_size_leaf,
          hjust = h_just,
          vjust = v_just
        )
    }
  }

  # Y-axis (level labels). Keep default expansion — labels render
  # in the margin via clip = "off", not by distorting the data scale.
  if (level_axis_labels) {
    lvl_labels <- paste0("L", seq_len(top_level))
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = heights,
        labels = lvl_labels,
        sec.axis = ggplot2::dup_axis()
      )
  }

  # Theme adjustments. Extend the bottom (or right, under coord_flip)
  # margin to fit rotated leaf labels. Size is computed in mm from the
  # longest label; coord_cartesian(clip = "off") lets geom_text escape
  # the panel into this margin.
  margin_pad_mm <- max(6, label_margin_mm)
  plot_margin <- if (vertical) {
    ggplot2::margin(t = 6, r = 10, b = margin_pad_mm, l = 10)
  } else {
    ggplot2::margin(t = 6, r = margin_pad_mm, b = 6, l = 10)
  }

  p <- p +
    ggplot2::theme(
      legend.position = "none",
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 12, hjust = 0.5),
      plot.margin = plot_margin
    )
  if (leaf_label_position == "below") {
    p <- p +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  # Titles
  p <- p + ggplot2::labs(title = title, subtitle = subtitle)

  # Orientation flip. clip = "off" ensures geom_text extending past the
  # panel boundary still renders into the expanded margin.
  if (vertical) {
    p <- p + ggplot2::coord_cartesian(clip = "off")
  } else {
    p <- p + ggplot2::coord_flip(clip = "off")
  }

  p
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
#' @param color_palette Character string specifying the color palette. Default is
#'   \code{"okabe-ito"} (CVD-safe) for categorical data, \code{"RdYlBu"} for
#'   continuous. Also accepts RColorBrewer names or viridis options.
#' @param theme_style Character string specifying the plot theme: "minimal"
#'   (default), "classic", or "void".
#' @param title Character string for plot title. Default is auto-generated
#'   based on plot type.
#' @param show_labels Logical indicating whether to show segment labels.
#'   Default is TRUE.
#' @param label_size Numeric size for labels. Default is 3.
#' @param segment_naming Specifies how to name segments in the visualization. Can be:
#'   \itemize{
#'     \item Character string: "auto" (default), "concat", "pattern", or "custom" -
#'       these are passed to \code{\link{segment.membership.enhanced}} for automatic naming
#'     \item data.frame: Custom segment labels with required columns "name" and "segment_label".
#'       \strong{This parameter accepts the direct output from \code{\link{segment.membership.enhanced}},}
#'       making it easy to maintain consistent segment naming across all visualizations.
#'       The data.frame must have:
#'       \itemize{
#'         \item "name": Node names from the original mobility matrix
#'         \item "segment_label": Meaningful segment names (e.g., "Executive_Leadership")
#'       }
#'     \item NULL: Uses default "auto" strategy
#'   }
#'   \strong{Recommended workflow:} Generate enhanced membership with
#'   \code{enhanced <- segment.membership.enhanced(segments, naming_strategy = "pattern")}
#'   and pass directly: \code{plot_segment_quality(segments, segment_naming = enhanced)}.
#'   This ensures consistent, meaningful segment names across all plot types and visualizations.
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
#' \dontrun{
#' # Generate data and run MONECA
#' mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 3)
#'
#' # Overview of level 2 quality
#' plot_segment_quality(seg)
#'
#' # Radar plot for segment comparison
#' plot_segment_quality(seg, plot_type = "radar", level = 2)
#' }
#'
#' @seealso
#' \code{\link{segment.quality}} for the underlying metrics,
#' \code{\link{moneca}} for the main analysis function,
#' \code{\link{plot_moneca_ggraph}} for network visualization
#'
#' @export
plot_segment_quality <- function(
  segments,
  plot_type = "overview",
  level = 2,
  metrics = NULL,
  color_palette = NULL,
  theme_style = "minimal",
  title = NULL,
  show_labels = TRUE,
  label_size = 3,
  segment_naming = "auto"
) {
  # Input validation
  if (!inherits(segments, "moneca")) {
    stop("segments must be a moneca object created by the moneca() function")
  }

  # Canonical metadata (cached or computed once)
  meta <- .get_metadata(segments)

  # Get quality data
  quality_data <- segment.quality(segments, final.solution = FALSE)

  # Set default metrics if not provided
  if (is.null(metrics)) {
    metrics <- c(
      "within.mobility",
      "share.of.mobility",
      "Density",
      "Nodes",
      "Max.path",
      "share.of.total"
    )
  }

  # Set default color palette based on plot type
  if (is.null(color_palette)) {
    color_palette <- if (plot_type %in% c("heatmap", "cohesion")) {
      "RdYlBu"
    } else {
      "okabe-ito"
    }
  }

  # Apply theme
  theme_base <- switch(
    theme_style,
    "minimal" = ggplot2::theme_minimal(),
    "classic" = ggplot2::theme_classic(),
    "void" = ggplot2::theme_void(),
    ggplot2::theme_minimal()
  )

  # Generate plots based on type
  if (plot_type == "overview") {
    # Multi-panel overview
    if (is.null(level)) {
      level <- 2
    }

    # Extract data for specified level
    level_cols <- grep(
      paste0("^", level, ": "),
      colnames(quality_data),
      value = TRUE
    )

    # Check if we found any columns for this level
    if (length(level_cols) == 0) {
      # Extract available levels from column names
      all_level_cols <- grep("^[0-9]+:", colnames(quality_data), value = TRUE)
      available_levels <- unique(gsub(":.*", "", all_level_cols))
      stop(paste(
        "No data found for level",
        level,
        ". Available levels:",
        paste(available_levels, collapse = ", ")
      ))
    }

    # Ensure we get a data frame even with single column
    level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]

    # Clean column names
    colnames(level_data) <- gsub(
      paste0("^", level, ": "),
      "",
      colnames(level_data)
    )

    # Apply segment naming for better labels
    if ("Segment" %in% colnames(level_data)) {
      segment_labels <- create_segment_labels(
        segments,
        level,
        level_data$Segment,
        segment_naming
      )
      level_data$segment_label <- segment_labels
      # Use segment labels instead of membership for x-axis
      level_data$plot_label <- level_data$segment_label
    } else {
      # Fallback to membership if Segment column not available
      level_data$plot_label <- level_data$Membership
    }

    # Create individual plots
    plots <- list()

    # 1. Within-mobility bar chart
    p1 <- ggplot2::ggplot(
      level_data,
      ggplot2::aes(x = plot_label, y = within.mobility)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        fill = .overview_bar_colors["cohesion"]
      ) +
      ggplot2::geom_hline(
        yintercept = 0.7,
        linetype = "dashed",
        color = .ref_line_high,
        alpha = 0.5
      ) +
      ggplot2::labs(
        x = "Segment",
        y = "Within-mobility",
        title = "Segment Cohesion"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (show_labels && nrow(level_data) <= 10) {
      p1 <- p1 +
        ggplot2::geom_text(
          ggplot2::aes(label = round(within.mobility, 2)),
          vjust = -0.5,
          size = label_size
        )
    }
    plots[[1]] <- p1

    # 2. Segment size (nodes)
    p2 <- ggplot2::ggplot(level_data, ggplot2::aes(x = plot_label, y = Nodes)) +
      ggplot2::geom_bar(
        stat = "identity",
        fill = .overview_bar_colors["size"]
      ) +
      ggplot2::labs(
        x = "Segment",
        y = "Number of Nodes",
        title = "Segment Size"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (show_labels && nrow(level_data) <= 10) {
      p2 <- p2 +
        ggplot2::geom_text(
          ggplot2::aes(label = Nodes),
          vjust = -0.5,
          size = label_size
        )
    }
    plots[[2]] <- p2

    # 3. Network density
    p3 <- ggplot2::ggplot(
      level_data,
      ggplot2::aes(x = plot_label, y = Density)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        fill = .overview_bar_colors["density"]
      ) +
      ggplot2::geom_hline(
        yintercept = 0.5,
        linetype = "dashed",
        color = .ref_line_high,
        alpha = 0.5
      ) +
      ggplot2::labs(
        x = "Segment",
        y = "Network Density",
        title = "Internal Connectivity"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (show_labels && nrow(level_data) <= 10) {
      p3 <- p3 +
        ggplot2::geom_text(
          ggplot2::aes(label = round(Density, 2)),
          vjust = -0.5,
          size = label_size
        )
    }
    plots[[3]] <- p3

    # 4. Share of mobility
    p4 <- ggplot2::ggplot(
      level_data,
      ggplot2::aes(x = plot_label, y = share.of.mobility)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        fill = .overview_bar_colors["share"]
      ) +
      ggplot2::labs(
        x = "Segment",
        y = "Share of Total Mobility",
        title = "Relative Importance"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    if (show_labels && nrow(level_data) <= 10) {
      p4 <- p4 +
        ggplot2::geom_text(
          ggplot2::aes(label = round(share.of.mobility, 2)),
          vjust = -0.5,
          size = label_size
        )
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
    # Cohesion vs size scatter plot - use final solution data (already aggregated)
    # Get quality data with final.solution = TRUE to get one row per final segment
    final_quality_data <- segment.quality(
      segments,
      final.solution = TRUE,
      segment_naming = segment_naming
    )

    # Validate that we have the expected columns
    if (!"Segment" %in% colnames(final_quality_data)) {
      stop("Expected 'Segment' column not found in final quality data")
    }

    # Check for required metrics
    required_cols <- c(
      "within.mobility",
      "share.of.mobility",
      "Density",
      "Nodes",
      "Max.path",
      "share.of.total"
    )
    missing_cols <- setdiff(required_cols, colnames(final_quality_data))
    if (length(missing_cols) > 0) {
      stop(paste(
        "Required columns missing from final quality data:",
        paste(missing_cols, collapse = ", ")
      ))
    }

    level_data <- final_quality_data

    # Validation: Ensure we have exactly one row per unique segment (should be guaranteed by final.solution=TRUE)
    if (length(unique(level_data$Segment)) != nrow(level_data)) {
      stop(
        "Final solution data has multiple rows for the same segment - this should not happen"
      )
    }

    # Use segment_label if available, otherwise create fallback labels
    if (!"segment_label" %in% colnames(level_data)) {
      level_data$segment_label <- paste("Segment", level_data$Segment)
    }

    # Additional validation: Ensure unique segment labels for plotting
    if (length(unique(level_data$segment_label)) != nrow(level_data)) {
      warning(
        "Multiple segments have the same label - this may cause plotting issues"
      )
      # Add suffix to make labels unique
      level_data$segment_label <- make.unique(
        level_data$segment_label,
        sep = " ("
      )
    }

    # Create scatter plot
    p <- ggplot2::ggplot(
      level_data,
      ggplot2::aes(
        x = Nodes,
        y = within.mobility,
        size = share.of.mobility,
        color = segment_label
      )
    ) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::scale_size_continuous(
        range = c(3, 10),
        name = "Share of Mobility"
      ) +
      ggplot2::geom_hline(
        yintercept = 0.7,
        linetype = "dashed",
        color = .ref_line_high,
        alpha = 0.5
      ) +
      ggplot2::geom_vline(
        xintercept = 2,
        linetype = "dashed",
        color = .ref_line_low,
        alpha = 0.5
      ) +
      ggplot2::labs(
        x = "Number of Nodes",
        y = "Within-mobility (Cohesion)",
        title = title %||% "Final Segment Quality Analysis",
        subtitle = "Ideal segments: upper-right quadrant"
      ) +
      theme_base

    # Add labels if requested - use segment labels instead of membership
    if (show_labels) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(label = segment_label),
          vjust = -1,
          size = label_size
        )
    }

    # Color palette - Remove the Membership legend since labels are on points
    n_segments <- nrow(level_data)
    p <- .apply_discrete_color_scale(p, color_palette, n_segments)

    return(p)
  } else if (plot_type == "radar") {
    # Radar plot
    if (is.null(level)) {
      level <- 2
    }

    # Extract and prepare data
    level_cols <- grep(
      paste0("^", level, ": "),
      colnames(quality_data),
      value = TRUE
    )

    # Check if we found any columns for this level
    if (length(level_cols) == 0) {
      # Extract available levels from column names
      all_level_cols <- grep("^[0-9]+:", colnames(quality_data), value = TRUE)
      available_levels <- unique(gsub(":.*", "", all_level_cols))
      stop(paste(
        "No data found for level",
        level,
        ". Available levels:",
        paste(available_levels, collapse = ", ")
      ))
    }

    level_data <- quality_data[, c("Membership", level_cols), drop = FALSE]
    colnames(level_data) <- gsub(
      paste0("^", level, ": "),
      "",
      colnames(level_data)
    )

    # Select only requested metrics
    metric_cols <- intersect(metrics, colnames(level_data))
    radar_data <- level_data[, c("Membership", metric_cols), drop = FALSE]

    # Apply segment naming for better labels
    if ("Segment" %in% colnames(level_data)) {
      segment_labels <- create_segment_labels(
        segments,
        level,
        level_data$Segment,
        segment_naming
      )
      radar_data$segment_label <- segment_labels
    } else {
      # Create numeric-based labels if Segment column not available
      radar_data$segment_label <- paste("Segment", 1:nrow(radar_data))
    }

    # Normalize metrics to 0-1 scale
    for (col in metric_cols) {
      if (col != "Membership") {
        col_data <- radar_data[[col]]
        radar_data[[col]] <- (col_data - min(col_data, na.rm = TRUE)) /
          (max(col_data, na.rm = TRUE) - min(col_data, na.rm = TRUE))
      }
    }

    # Reshape data for plotting
    radar_long <- tidyr::pivot_longer(
      radar_data,
      cols = -c(Membership, segment_label),
      names_to = "Metric",
      values_to = "Value"
    )

    # Create radar plot using coord_polar
    p <- ggplot2::ggplot(
      radar_long,
      ggplot2::aes(
        x = Metric,
        y = Value,
        group = segment_label,
        color = segment_label
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::coord_polar() +
      ggplot2::ylim(0, 1) +
      ggplot2::labs(
        title = title %||% paste("Segment Profiles - Level", level),
        subtitle = "Normalized metrics (0-1 scale)"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10))

    # Color palette
    n_segments <- length(unique(radar_long$segment_label))
    p <- .apply_discrete_color_scale(
      p,
      color_palette,
      n_segments,
      guide = ggplot2::guide_legend()
    )

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
    heatmap_data <- quality_data[,
      c("Membership", all_metric_cols),
      drop = FALSE
    ]

    # Apply segment naming for better labels
    if (meta$n_levels > 0) {
      # Use metadata-based membership at level 1 for labelling
      level1_membership <- .membership_from_metadata(meta, 1)

      # Create a mapping from membership ID to label
      membership_mapping <- data.frame(
        Membership = unique(heatmap_data$Membership),
        stringsAsFactors = FALSE
      )
      membership_mapping$segment_label <- level1_membership$segment_label[
        match(membership_mapping$Membership, level1_membership$membership)
      ]

      # Handle potential NA values by falling back to membership
      membership_mapping$segment_label[is.na(
        membership_mapping$segment_label
      )] <-
        membership_mapping$Membership[is.na(membership_mapping$segment_label)]

      heatmap_data$segment_label <- membership_mapping$segment_label[match(
        heatmap_data$Membership,
        membership_mapping$Membership
      )]
    } else {
      heatmap_data$segment_label <- heatmap_data$Membership
    }

    # Reshape to long format, excluding both Membership and segment_label
    heatmap_long <- tidyr::pivot_longer(
      heatmap_data,
      cols = -c(Membership, segment_label),
      names_to = "Level_Metric",
      values_to = "Value"
    )

    # Separate level and metric
    heatmap_long$Level <- gsub(": .*", "", heatmap_long$Level_Metric)
    heatmap_long$Metric <- gsub("^[0-9]+: ", "", heatmap_long$Level_Metric)

    # FIX: Create more informative x-axis labels instead of just "1", "2", "3"
    # Convert numeric levels to descriptive labels
    heatmap_long$Level_Label <- paste("Level", heatmap_long$Level)

    # Add segment labels to long format data
    heatmap_long$segment_label <- heatmap_data$segment_label[match(
      heatmap_long$Membership,
      heatmap_data$Membership
    )]

    # Create heatmap
    p <- ggplot2::ggplot(
      heatmap_long,
      ggplot2::aes(x = Level_Label, y = segment_label, fill = Value)
    ) +
      ggplot2::geom_tile() +
      ggplot2::facet_wrap(~Metric, scales = "free", ncol = 3) +
      ggplot2::scale_fill_distiller(palette = color_palette, direction = 1) +
      ggplot2::labs(
        title = title %||% "Segment Quality Metrics Across Levels",
        x = "Hierarchical Level",
        y = "Segment"
      ) +
      theme_base +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    return(p)
  } else if (plot_type == "evolution") {
    # Evolution across levels

    # Prepare data for evolution plot
    evolution_data <- list()

    for (m in metrics) {
      metric_cols <- grep(
        paste0(": ", m, "$"),
        colnames(quality_data),
        value = TRUE
      )
      if (length(metric_cols) > 0) {
        metric_data <- quality_data[,
          c("Membership", metric_cols),
          drop = FALSE
        ]

        # Reshape to long format
        metric_long <- tidyr::pivot_longer(
          metric_data,
          cols = -Membership,
          names_to = "Level",
          values_to = "Value"
        )
        metric_long$Level <- as.numeric(gsub(": .*", "", metric_long$Level))
        metric_long$Metric <- m

        evolution_data[[m]] <- metric_long
      }
    }

    # Combine all metrics
    evolution_combined <- do.call(rbind, evolution_data)

    # Apply segment naming for better labels
    if (nrow(evolution_combined) > 0 && length(segments$segment.list) > 0) {
      # Create segment labels mapping
      unique_memberships <- unique(evolution_combined$Membership)
      segment_labels <- create_segment_labels(
        segments,
        1,
        1:length(unique_memberships),
        segment_naming
      )

      # Create mapping
      membership_mapping <- data.frame(
        Membership = unique_memberships,
        segment_label = segment_labels[1:length(unique_memberships)]
      )

      # Apply mapping
      evolution_combined$segment_label <- membership_mapping$segment_label[match(
        evolution_combined$Membership,
        membership_mapping$Membership
      )]
    } else {
      evolution_combined$segment_label <- evolution_combined$Membership
    }

    # Create line plot
    p <- ggplot2::ggplot(
      evolution_combined,
      ggplot2::aes(
        x = Level,
        y = Value,
        color = segment_label,
        group = segment_label
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::facet_wrap(~Metric, scales = "free_y", ncol = 2) +
      ggplot2::labs(
        title = title %||% "Metric Evolution Across Hierarchical Levels",
        x = "Hierarchical Level",
        y = "Metric Value"
      ) +
      theme_base

    # Color palette
    n_segments <- length(unique(evolution_combined$segment_label))
    p <- .apply_discrete_color_scale(
      p,
      color_palette,
      n_segments,
      guide = ggplot2::guide_legend()
    )

    return(p)
  } else {
    stop(
      "Invalid plot_type. Choose 'overview', 'cohesion', 'radar', 'heatmap', or 'evolution'"
    )
  }
}
