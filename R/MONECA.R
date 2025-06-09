#' MONECA: Mobility Network Clustering Analysis
#'
#' MONECA creates a weighted network from a mobility table and uses cliques
#' to create discrete and nested clusters. The package provides tools for
#' analyzing social mobility patterns through graph-theoretic approaches.
#' 
#' @details
#' The MONECA package implements a sophisticated algorithm for detecting
#' mobility patterns in social networks. It identifies groups with high
#' internal mobility (oversandsynlig mobilitet) by finding cliques in the
#' weighted network representation of mobility data.
#' 
#' Key features:
#' \itemize{
#'   \item Network-based clustering of mobility tables
#'   \item Hierarchical segmentation at multiple levels
#'   \item Various visualization methods (base R and ggplot2)
#'   \item Quality metrics for assessing segmentation
#' }
#' 
#' Main functions:
#' \itemize{
#'   \item \code{\link{moneca}}: Core clustering algorithm
#'   \item \code{\link{find.segments}}: Segment identification
#'   \item \code{\link{gg.moneca}}: ggplot2-based visualization
#'   \item \code{\link{moneca.plot}}: Base R visualization
#' }
#' 
#' @section Package options:
#' MONECA uses the following options:
#' \itemize{
#'   \item \code{moneca.verbose}: Logical, controls verbosity of output (default: FALSE)
#'   \item \code{moneca.color.scheme}: Character, default color scheme for plots (default: "Set3")
#' }
#' 
#' @section Dependencies:
#' MONECA depends on several packages:
#' \itemize{
#'   \item \code{igraph}: For network analysis (compatible with versions >= 1.3.0)
#'   \item \code{ggplot2}: For advanced plotting
#'   \item \code{RColorBrewer}: For color palettes
#'   \item \code{scales}: For scale transformations
#' }
#' 
#' @docType package
#' @name MONECA
#' @aliases MONECA-package
#' 
#' @examples 
#' # Generate synthetic mobility data
#' mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
#' 
#' # Run basic MONECA analysis
#' seg <- moneca(mobility_data, segment.levels = 3)
#' print(seg)
#' 
#' # Visualize results with modern ggraph plotting
#' \dontrun{
#' # Modern network visualization
#' plot_moneca_ggraph(seg, title = "MONECA Network Analysis")
#' 
#' # Ego network analysis
#' plot_ego_ggraph(seg, mobility_data, ego_id = 3)
#' 
#' # Legacy plotting (still available)
#' moneca.plot(seg)
#' gg.moneca(seg)
#' }
#' 
#' @author Jonas Touboel \email{jt@@soc.ku.dk}
#' @author Anton Grau Larsen \email{agl.ioa@@cbs.dk} (maintainer)
#' 
#' @references
#' Touboel, J. (2018). "Mobility Network Clustering Analysis: 
#' A Graph-Theoretic Approach to Social Mobility." \emph{Journal of Social Structure}.
#' 
#' @import ggplot2
#' @import igraph
#' @import grid
#' @import RColorBrewer
#' @import scales
#' @import toOrdinal
#' @importFrom stats cor dist hclust cutree
#' @importFrom grDevices rgb col2rgb
NULL

