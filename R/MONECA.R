#' moneca: Mobility Network Clustering Analysis
#'
#' moneca creates weighted networks from mobility tables and uses cliques to identify
#' discrete and nested clusters of social positions with high internal mobility.
#' The package provides comprehensive tools for analyzing social mobility patterns
#' through graph-theoretic approaches with modern visualization capabilities.
#'
#' @details
#' The moneca package implements a sophisticated hierarchical clustering algorithm
#' that detects mobility patterns in social networks. It identifies segments with
#' high internal mobility by finding cliques in weighted network representations
#' of mobility data, creating nested hierarchical structures that reveal the
#' underlying organization of social mobility.
#'
#' \strong{Key Features:}
#' \itemize{
#'   \item \strong{Network-based clustering} using relative risk matrices
#'   \item \strong{Hierarchical segmentation} with multiple nested levels
#'   \item \strong{Modern visualization} with ggraph and ggplot2
#'   \item \strong{Synthetic data generation} for testing and demonstrations
#'   \item \strong{Quality metrics} for assessing segmentation performance
#'   \item \strong{Multiple visualization types} (network, ego, stair plots)
#'   \item \strong{Flexible input formats} with automatic data validation
#' }
#'
#' \strong{Core Functions:}
#' \itemize{
#'   \item \code{\link{moneca}}: Main clustering algorithm
#'   \item \code{\link{find.segments}}: Core segmentation identification
#'   \item \code{\link{weight.matrix}}: Relative risk matrix calculation
#'   \item \code{\link{segment.membership}}: Extract cluster memberships
#' }
#'
#' \strong{Modern Visualization (Recommended):}
#' \itemize{
#'   \item \code{\link{plot_moneca_ggraph}}: Network visualization with ggraph
#'   \item \code{\link{plot_ego_ggraph}}: Ego network analysis
#'   \item \code{\link{plot_stair_ggraph}}: Multi-level segmentation plots
#' }
#'
#' \strong{Legacy Visualization (Backward Compatibility):}
#' \itemize{
#'   \item \code{\link{gg.moneca}}: Original ggplot2-based visualization
#'   \item \code{\link{moneca.plot}}: Base R network plotting
#'   \item \code{\link{ego.plot}}: Legacy ego network plots
#'   \item \code{\link{stair.plot}}: Legacy stair plots
#' }
#'
#' \strong{Data Generation:}
#' \itemize{
#'   \item \code{\link{generate_mobility_data}}: Create synthetic mobility tables
#' }
#'
#' @section Package Dependencies:
#' \strong{Required packages:}
#' \itemize{
#'   \item \code{igraph} (>= 1.3.0): Network analysis with compatibility layer
#'   \item \code{ggplot2} (>= 3.4.0): Graphics and plotting
#'   \item \code{RColorBrewer}: Color palettes for visualizations
#'   \item \code{scales}: Scale transformations and formatting
#' }
#'
#' \strong{Modern visualization packages:}
#' \itemize{
#'   \item \code{ggraph} (>= 2.0.0): Advanced network visualization
#'   \item \code{tidygraph} (>= 1.2.0): Tidy graph manipulation
#'   \item \code{dplyr} (>= 1.0.0): Data manipulation
#' }
#'
#' \strong{Additional packages:}
#' \itemize{
#'   \item \code{grid}: Low-level graphics
#'   \item \code{toOrdinal}: Ordinal number formatting
#' }
#'
#' @section Color Vision Deficiency (CVD) Accessibility:
#' All modern visualization functions use CVD-safe defaults:
#' \itemize{
#'   \item Default discrete palette is the 8-color Okabe-Ito palette
#'     (\code{color_palette = "okabe-ito"}), distinguishable under deuteranopia,
#'     protanopia, and tritanopia.
#'   \item Continuous gradients use perceptually uniform viridis scales.
#'   \item Diverging scales use a PuOr (purple-orange) scheme that remains
#'     discriminable under common CVD types.
#'   \item \code{plot_moneca_ggraph()} supports \code{node_shape = "segment"} for
#'     shape-based redundant encoding alongside color.
#'   \item All hardcoded fill and reference-line colors use CVD-safe hex values.
#' }
#' Users who prefer other palettes can override with \code{color_palette = "Set3"}
#' or any RColorBrewer/viridis palette name.
#'
#'
"_PACKAGE"
#'
#' @examples
#' # Generate synthetic mobility data
#' mobility_data <- generate_mobility_data(n_classes = 6, seed = 42)
#'
#' # Run moneca analysis
#' seg <- moneca(mobility_data, segment.levels = 3)
#' print(seg)
#'
#' # Extract segment membership
#' membership <- segment.membership(seg)
#' print(membership)
#'
#' # Modern visualization (recommended)
#' \dontrun{
#' # Basic network plot
#' plot_moneca_ggraph(seg, title = "Social Mobility Network")
#'
#' # Customized network visualization
#' plot_moneca_ggraph(seg,
#'                   layout = "stress",
#'                   node_color = "mobility",
#'                   edge_width = "weight",
#'                   color_palette = "Spectral")
#'
#' # Ego network analysis
#' plot_ego_ggraph(seg, mobility_data, ego_id = 3,
#'                title = "Mobility from Middle Class")
#'
#' # Multi-level stair plot
#' plot_stair_ggraph(seg)
#' }
#'
#' # Create different mobility scenarios
#' rigid_data <- generate_mobility_data(n_classes = 8, immobility_strength = 0.8, seed = 100)
#' fluid_data <- generate_mobility_data(n_classes = 8, immobility_strength = 0.3, seed = 200)
#'
#' \dontrun{
#' # Compare different mobility regimes
#' rigid_seg <- moneca(rigid_data, segment.levels = 2)
#' plot_moneca_ggraph(rigid_seg, title = "Rigid Mobility Structure")
#' }
#'
#' @author Jonas Touboel \email{jt@@soc.ku.dk} (original algorithm and methodology)
#' @author Anton Grau Larsen \email{agl.ioa@@cbs.dk} (original package development)
#' @author Giampaolo Montaletti \email{giampaolo.montaletti@@gmail.com} (maintainer)
#'
#' @references
#' Touboel, J., & Larsen, A. G. (2017). Mapping the Social Class Structure:
#' From Occupational Mobility to Social Class Categories Using Network Analysis.
#' \emph{Sociology}, 51(6), 1257-1276. \doi{10.1177/0038038516673125}
#'
#' Larsen, A. G., & Touboel, J. (2015). Social network analysis of occupational
#' mobility and social class. In \emph{Research Handbook on Analytical Sociology}
#' (pp. 411-434). Edward Elgar Publishing.
#'
#' @import ggplot2
#' @import igraph
#' @import grid
#' @import RColorBrewer
#' @import scales
#' @import toOrdinal
#' @importFrom stats cor dist hclust cutree na.omit aggregate median quantile rgamma runif var
#' @importFrom grDevices rgb col2rgb chull
#' @importFrom utils tail setTxtProgressBar txtProgressBar packageVersion head
#' @importFrom scales percent_format
#' @importFrom tidyr pivot_longer
#' @name _PACKAGE
#' @aliases moneca-package
NULL
