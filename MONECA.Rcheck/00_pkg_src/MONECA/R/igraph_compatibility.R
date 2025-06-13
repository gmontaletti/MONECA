#' igraph Compatibility Layer
#' 
#' This file provides compatibility functions to handle both old and new igraph API
#' ensuring MONECA works with igraph versions from 1.3.0 onwards
#' 
#' @keywords internal

# Store compatibility functions in package environment
.moneca_env <- new.env(parent = emptyenv())

# Check igraph version and set up appropriate function mappings
.setup_igraph_compat <- function() {
  igraph_version <- utils::packageVersion("igraph")
  
  # Version 1.3.0 introduced the new API with underscore notation
  if (igraph_version >= "1.3.0") {
    # Use new API functions
    .moneca_env$moneca_graph_from_adjacency <- function(...) igraph::graph_from_adjacency_matrix(...)
    .moneca_env$moneca_get_edgelist <- function(...) igraph::as_edgelist(...)
    .moneca_env$moneca_graph_density <- function(...) igraph::edge_density(...)
    .moneca_env$moneca_shortest_paths <- function(...) igraph::distances(...)
    .moneca_env$moneca_average_path_length <- function(...) igraph::mean_distance(...)
    .moneca_env$moneca_components <- function(...) igraph::components(...)
  } else {
    # Use old API functions
    .moneca_env$moneca_graph_from_adjacency <- function(...) igraph::graph.adjacency(...)
    .moneca_env$moneca_get_edgelist <- function(...) igraph::get.edgelist(...)
    .moneca_env$moneca_graph_density <- function(...) igraph::graph.density(...)
    .moneca_env$moneca_shortest_paths <- function(...) igraph::shortest.paths(...)
    .moneca_env$moneca_average_path_length <- function(...) igraph::average.path.length(...)
    .moneca_env$moneca_components <- function(...) igraph::clusters(...)
  }
  
  # Handle norm_coords which was removed in newer versions
  if (exists("norm_coords", where = asNamespace("igraph"), mode = "function")) {
    .moneca_env$moneca_norm_coords <- function(...) igraph::norm_coords(...)
  } else {
    .moneca_env$moneca_norm_coords <- function(layout, xmin = -1, xmax = 1, ymin = -1, ymax = 1) {
      # Normalize layout coordinates to specified range
      if (ncol(layout) != 2) {
        stop("Layout must be a two-column matrix")
      }
      
      # Scale x coordinates
      x_range <- range(layout[, 1])
      if (x_range[1] != x_range[2]) {
        layout[, 1] <- (layout[, 1] - x_range[1]) / 
                       (x_range[2] - x_range[1]) * 
                       (xmax - xmin) + xmin
      }
      
      # Scale y coordinates  
      y_range <- range(layout[, 2])
      if (y_range[1] != y_range[2]) {
        layout[, 2] <- (layout[, 2] - y_range[1]) / 
                       (y_range[2] - y_range[1]) * 
                       (ymax - ymin) + ymin
      }
      
      return(layout)
    }
  }
}

# Initialize compatibility functions when loaded
.setup_igraph_compat()

# Export wrapper functions that access the environment
moneca_graph_from_adjacency <- function(...) .moneca_env$moneca_graph_from_adjacency(...)
moneca_get_edgelist <- function(...) .moneca_env$moneca_get_edgelist(...)
moneca_graph_density <- function(...) .moneca_env$moneca_graph_density(...)
moneca_shortest_paths <- function(...) .moneca_env$moneca_shortest_paths(...)
moneca_average_path_length <- function(...) .moneca_env$moneca_average_path_length(...)
moneca_components <- function(...) .moneca_env$moneca_components(...)
moneca_norm_coords <- function(...) .moneca_env$moneca_norm_coords(...)