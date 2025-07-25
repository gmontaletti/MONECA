% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modern_plotting.R
\name{plot_moneca_dendrogram}
\alias{plot_moneca_dendrogram}
\title{Plot MONECA Results as Dendrogram}
\usage{
plot_moneca_dendrogram(
  segments,
  height_method = "uniform",
  color_segments = TRUE,
  show_labels = TRUE,
  label_size = 3,
  branch_width = 1,
  title = "MONECA Hierarchical Clustering",
  subtitle = NULL,
  color_palette = "Set3",
  theme_style = "minimal",
  vertical = TRUE
)
}
\arguments{
\item{segments}{A MONECA object returned by \code{\link{moneca}}.}

\item{height_method}{Character string specifying how to calculate dendrogram heights:
\itemize{
  \item "uniform" (default): Equal spacing between levels
  \item "mobility": Height based on mobility reduction between levels
  \item "segments": Height based on number of segments at each level
}}

\item{color_segments}{Logical indicating whether to color branches by final
segment membership. Default is TRUE.}

\item{show_labels}{Logical indicating whether to show category labels at the
bottom. Default is TRUE.}

\item{label_size}{Numeric size for labels. Default is 3.}

\item{branch_width}{Numeric width for dendrogram branches. Default is 1.}

\item{title}{Character string for plot title. Default is "MONECA Hierarchical Clustering".}

\item{subtitle}{Character string for plot subtitle. Default is NULL.}

\item{color_palette}{Character string specifying the RColorBrewer palette for
segment colors. Default is "Set3".}

\item{theme_style}{Character string specifying the plot theme. Options are
"minimal" (default), "classic", or "void".}

\item{vertical}{Logical indicating whether to plot vertically (TRUE, default)
or horizontally (FALSE).}
}
\value{
A ggplot2 object representing the dendrogram.
}
\description{
Creates a dendrogram-like visualization of the hierarchical clustering results
from MONECA analysis. This function shows how categories are progressively
aggregated across segmentation levels, making the hierarchical structure clear.
}
\details{
This function creates a dendrogram visualization that clearly shows:
\itemize{
  \item How individual categories (leaves) are grouped at each level
  \item The hierarchical relationships between segments
  \item The progressive aggregation from individual categories to larger segments
}

The dendrogram branches show merging points where categories or segments are
combined based on the MONECA algorithm's clique detection. Unlike traditional
hierarchical clustering, MONECA can create non-binary trees where multiple
categories merge simultaneously.

\strong{No-Crossing Layout:} Categories on the x-axis are automatically ordered
using a hierarchical algorithm that completely eliminates line crossings between
all levels. The algorithm processes segments from the most aggregated level down,
ensuring that at each level, categories are grouped optimally to prevent any
crossing lines. This creates the clearest possible visualization of the 
hierarchical structure.
}
\examples{
# Generate synthetic data and run MONECA
mobility_data <- generate_mobility_data(n_classes = 8, seed = 123)
seg <- moneca(mobility_data, segment.levels = 3)

# Basic dendrogram
plot_moneca_dendrogram(seg)

# Dendrogram with mobility-based heights
plot_moneca_dendrogram(seg, height_method = "mobility", 
                      title = "Mobility-based Hierarchical Clustering")

# Horizontal dendrogram without colors
plot_moneca_dendrogram(seg, vertical = FALSE, color_segments = FALSE)

# Customize appearance
plot_moneca_dendrogram(seg, 
                      color_palette = "Dark2",
                      branch_width = 1.5,
                      label_size = 4,
                      theme_style = "classic")

}
\seealso{
\code{\link{moneca}} for the main analysis function,
\code{\link{plot_moneca_ggraph}} for network visualization,
\code{\link{plot_stair_ggraph}} for multi-level visualization
}
