#########################################
### GGPLOTTING
# Vi skal kunne angive styrken af den interne mobilitet i farveskalaen (nok i virkeligheden alpha) for borders

# library(igraph)
# library(ggplot2)
#source("~/My Dropbox/R/Elite/soc.sna//soc.sna.R")
# layout             = layout.matrix(segments)
# 
# layout <- lay
# level             = seq(segments$segment.list)
# 
# edges              = log(segment.edges(segments)+1)
# mode               = "directed"
# 
# vertex.size        = "row.total"
# vertex.fill        = "segment"
# vertex.alpha       = 1
# vertex.color       = "black"
# vertex.shape       = 21
# 
# show.edges         = TRUE
# edge.size          = 1
# edge.alpha         = "weight"
# edge.color         = "weight"
# edge.line          = "solid"
# 
# show.text          = TRUE
# text.size          = 3
# text.color        = "black"
# text.alpha         = 1
# text.vjust         = 1.5
# 
# show.borders       = TRUE
# border.size        = 1
# border.fill        = NA
# border.color       = "black"
# border.alpha       = 1
# border.padding     = 1
# 
# legend             = "side"
# 
# border.text        = TRUE
# border.text.size   = 4
# border.text.color  = "black"
# border.text.vjust  = -0.2

#' Legacy ggplot2 Visualization for MONECA Objects
#' 
#' Creates network visualizations of MONECA clustering results using ggplot2.
#' This function provides extensive customization options but has been superseded
#' by \code{\link{plot_moneca_ggraph}} for most use cases.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param level Integer vector specifying which hierarchical levels to display.
#' @param layout Matrix of node coordinates or layout function result.
#' @param edges Edge matrix or transformed edge weights for visualization.
#' @param mode Character string specifying graph mode ("directed" or "undirected").
#' @param vertex.size Aesthetic for vertex size. Can be "total", "mobility", or numeric.
#' @param vertex.fill Aesthetic for vertex fill color. Can be "segment" or color specification.
#' @param vertex.alpha Numeric value (0-1) for vertex transparency.
#' @param vertex.color Color for vertex borders.
#' @param vertex.shape Numeric code for vertex shape (see ggplot2 shapes).
#' @param show.edges Logical indicating whether to display edges.
#' @param edge.size Size specification for edges.
#' @param edge.alpha Transparency for edges. Can be "weight" or numeric.
#' @param edge.color Color specification for edges. Can be "weight" or color name.
#' @param edge.line Line type for edges ("solid", "dashed", etc.).
#' @param show.text Logical indicating whether to show vertex labels.
#' @param text.size Numeric size for vertex labels.
#' @param text.color Color for vertex labels.
#' @param text.alpha Transparency for vertex labels.
#' @param text.vjust Vertical adjustment for labels.
#' @param show.borders Logical indicating whether to show segment boundaries.
#' @param border.size Size for segment borders.
#' @param border.fill Fill color for segment boundaries.
#' @param border.color Color for segment border lines.
#' @param border.alpha Transparency for segment borders.
#' @param border.padding Padding around segment boundaries.
#' @param border.text Logical indicating whether to show segment labels.
#' @param border.labels Character vector of custom segment labels.
#' @param border.text.size Size for segment labels.
#' @param border.text.color Color for segment labels.
#' @param border.text.vjust Vertical adjustment for segment labels.
#' @param border.text.hjust Horizontal adjustment for segment labels.
#' @param midpoints Logical indicating whether to show edge midpoints.
#' @param midpoint.arrow Logical indicating whether to show arrows at midpoints.
#' @param edge.text Logical indicating whether to show edge labels.
#' @param edge.text.size Size for edge labels.
#' @param edge.text.alpha Transparency for edge labels.
#' @param legend Position for legend ("side", "bottom", "none", etc.).
#' 
#' @return A ggplot2 object.
#' 
#' @details
#' This function provides a highly customizable but complex interface for creating
#' MONECA visualizations. It requires the eliter package for some functionality.
#' For most users, \code{\link{plot_moneca_ggraph}} offers a more modern and
#' user-friendly interface with better defaults.
#' 
#' \strong{Note}: This function is maintained for backward compatibility but
#' is no longer actively developed. New features are added to the ggraph-based
#' plotting functions instead.
#' 
#' @examples 
#' \dontrun{
#' # Requires eliter package
#' data(occupations)
#' gg.moneca(mob.seg)
#' 
#' # Custom styling
#' gg.moneca(mob.seg, 
#'          vertex.fill = "red",
#'          edge.color = "blue",
#'          show.borders = FALSE)
#' }
#' 
#' @seealso 
#' \code{\link{plot_moneca_ggraph}} for modern ggraph-based plotting,
#' \code{\link{moneca.plot}} for base graphics plotting
#' 
#' @export
gg.moneca               <- function(segments,
                                   level             = seq(segments$segment.list),
                                   layout             = layout.matrix(segments),
                                   edges              = log(segment.edges(segments)+1),
                                   mode               = "directed",
                                   
                                   vertex.size        = "total",
                                   vertex.fill        = "segment",
                                   vertex.alpha       = 1,
                                   vertex.color       = "black",
                                   vertex.shape       = 21,
                                   
                                   show.edges         = TRUE,            
                                   edge.size          = 1,
                                   edge.alpha         = "weight",
                                   edge.color         = "weight",
                                   edge.line          = "solid",
                                                                      
                                   show.text          = TRUE,
                                   text.size          = 3,
                                   text.color        = "black",
                                   text.alpha         = 1,
                                   text.vjust         = 1.5,
                                   
                                   show.borders       = TRUE,
                                   border.size        = 1,
                                   border.fill        = NA,
                                   border.color       = "black",
                                   border.alpha       = 1,
                                   border.padding     = 0.7,
                                   border.text        = TRUE,
                                   border.labels      = "segments",
                                   border.text.size   = 4,
                                   border.text.color  = "black",
                                   border.text.vjust  = -0.2,
                                   border.text.hjust  = 1,
                                   
                                   midpoints          = TRUE,
                                   midpoint.arrow     = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"),
                                   
                                   edge.text          = FALSE,
                                   edge.text.size     = 3,
                                   edge.text.alpha    = 0.9,
                                                                      
                                   legend             = "side"
                                   ){

if(identical(border.labels, "segments")){
membership             <- segment.membership(segments, level = level)[,2]
layout                 <- data.frame(layout, membership = membership)
colnames(layout)       <- c("X", "Y", "Membership")
}

if(length(border.labels) == nrow(layout)){
  layout               <- data.frame(layout, membership = border.labels)
colnames(layout)       <- c("X", "Y", "Membership")
}
  
  
level                 <- level[level != 1]
seg                    <- segments
seg$segment.list       <- segments$segment.list[level]
seg$mat.list           <- segments$mat.list[level]
#segments               <- unlist(seg$segment.list, recursive=FALSE) # Det her er knald sort! Den bliver ikke brugt i sin unlistede form



mat.edges              <- edges
gra.edges              <- moneca_graph_from_adjacency(mat.edges, mode=mode, weighted=TRUE, diag = FALSE)                     

scale_modifications    <- list()

if(identical(edge.color, "weight")){
  edge.color  <- E(gra.edges)$weight
  scale_modifications$edge.color <- scale_color_continuous(high = "darkblue", low = "azure1")
}

if(identical(edge.alpha, "weight"))   edge.alpha   <- E(gra.edges)$weight

if(identical(vertex.fill, "segment")){
  vertex.fill  <- segment.membership(segments, level = level)$membership
  scale_modifications$vertex.fill <- scale_fill_discrete(guide = "none")
}

if(identical(vertex.size, "total")){
  mat                 <- segments$mat.list[[1]]
  totals              <- (mat[nrow(mat),] + mat[, nrow(mat)]) / 2
  totals              <- totals[-length(totals)]
  vertex.size         <- totals
  scale_modifications$vertex.size <- scale_size_continuous(range = c(4,10))
} 

if(identical(vertex.size, "col.total")){
  col.total           <- data.frame(t(segments$mat.list[[1]]))$Total
  vertex.size         <- row.total[-length(col.total)]
  scale_modifications$vertex.size <- scale_size_continuous(range = c(4,9))
} 


p                      <- eliter::graph.plot(gra.edges, layout=as.matrix(layout[, 1:2]),
                          vertex.color = vertex.color, vertex.fill = vertex.fill, vertex.shape = vertex.shape,
                          vertex.size = vertex.size, vertex.alpha = vertex.alpha,
                          edges = show.edges, edge.color = edge.color, edge.alpha = edge.alpha,
                          edge.size = edge.size, edge.line = edge.line, edge.order = FALSE,
                          text = show.text, text.size = text.size, text.color = text.color,
                          text.alpha = text.alpha, legend = legend, text.vjust = text.vjust,
                          midpoints = midpoints, midpoint.arrow = midpoint.arrow,
                          edge.text = edge.text, edge.text.size = edge.text.size, edge.text.alpha = edge.text.alpha
                          )

circleFun <- function(center = c(0, 0), diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

segment.circles.hull     <- function(layout, group, diameter){
x                        <- layout[group, 1:2]
membership.seg           <- unique(as.character(layout$Membership[group]))
list.of.circles          <- apply(x, 1, circleFun, diameter = diameter)
all.circle.coordinates   <- do.call(rbind, list.of.circles)
circle.hull              <- all.circle.coordinates[chull(all.circle.coordinates),]
cbind(circle.hull, group = runif(1, min = 0, max = 999999999), membership = membership.seg)
}

annotate_segments        <- function(layout, seg.list, diameter, border.alpha){
  segment.circles          <- lapply(seg.list, segment.circles.hull, layout = layout, diameter = max(layout[, 1:2]) / diameter)
  segment.circles          <- do.call(rbind, segment.circles)
  annotate(geom = "polygon", x = segment.circles$x, y=segment.circles$y, group = segment.circles$group, fill = NA, color = "black", alpha = border.alpha)
}

if(identical(show.borders, TRUE)){
list.annotate            <- lapply(seg$segment.list, annotate_segments, layout = layout, diameter = (1/border.padding) * 20, border.alpha = border.alpha)
p                        <- p + list.annotate
}

if(identical(border.text, TRUE) & length(level) > 0){
  
  border.padding.diameter  <- max(layout[, 1:2]) / ((1/border.padding) * 20)
  seg.circles              <- list()
  for ( i in 1:length(seg$segment.list)){
  segment.circles          <- lapply(seg$segment.list[[i]], segment.circles.hull, layout = layout, diameter =  border.padding.diameter)
  segment.circles          <- do.call(rbind, segment.circles)
  seg.circles[[i]]         <- segment.circles
  }
  segment.circles          <- do.call(rbind, seg.circles)
  max.circles              <- aggregate(segment.circles$y, by = list(segment.circles$membership), FUN = max)
  max.segments             <- segment.circles[(segment.circles$y %in% max.circles$x) & (segment.circles$membership %in% max.circles$Group.1),]
  
  max.segments$xend        <- max.segments$x + ((border.padding.diameter * 2) * (border.text.size / 3.9))
  
  list.annotate            <- list(annotate(geom = "text", x = max.segments$xend, y=max.segments$y, label = max.segments$membership, color = border.text.color, size = border.text.size, vjust = border.text.vjust, hjust = border.text.hjust),  
                                   annotate(geom = "segment", x = max.segments$x, xend = max.segments$xend , y = max.segments$y, yend = max.segments$y, color = border.color, alpha = border.alpha))
  
  
  p                        <- p + list.annotate
  
  
  # Annotate singles
  tab.mem                  <- table(layout$Membership)
  if(any(tab.mem == 1)){
  singles.layout           <- layout[layout$Membership %in% names(tab.mem)[tab.mem == 1],]
  singles.layout           <- data.frame(x = singles.layout$X, y = singles.layout$Y, group = runif(1, min = 0, max = 999999999), membership = singles.layout$Membership)
  singles.layout$y         <- singles.layout$y + (border.padding.diameter * 0.25)
  singles.layout$xend      <- singles.layout$x + ((border.padding.diameter * 2) * (border.text.size / 3.9))
  singles.layout$x         <- singles.layout$x + (border.padding.diameter * 0.25)
  
  list.annotate            <- list(annotate(geom = "text", x = singles.layout$xend, y=singles.layout$y, label = singles.layout$membership, color = border.text.color, size = border.text.size, vjust = border.text.vjust, hjust = border.text.hjust),  
                                   annotate(geom = "segment", x = singles.layout$x, xend = singles.layout$xend , y = singles.layout$y, yend = singles.layout$y, color = border.color, alpha = border.alpha))
  
  p                        <- p + list.annotate
  }
  
}


p + scale_modifications
}


##################################################
###  Graph plot
# graph.plot <- function(graph, layout = layout.fruchterman.reingold(graph),
#                        vertex.color = "black", vertex.fill = "grey60", vertex.shape = 21, vertex.size = 3, vertex.alpha = 1,
#                        edges = TRUE, edge.color = "black", edge.alpha = 0.2, edge.size = 1, edge.line = "solid", edge.order = FALSE,
#                        text = FALSE, text.size = 3, text.colour = "black", text.alpha = 1, legend = "side", text.vjust = 1.5, midpoints = FALSE,
#                        midpoint.arrow = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed")){
#   
#   
#   
#   vertex.coords           <- as.data.frame(vertex.coord(graph, layout))
#   
#   vertex.l                <- list(color=vertex.color, fill=vertex.fill, shape=vertex.shape, size=vertex.size, alpha=vertex.alpha)
#   v.i                     <- unlist(lapply(vertex.l, length)) == 1
#   vertex.attributes       <- vertex.l[v.i]
#   vertex.aes              <- vertex.l[v.i==FALSE]
#   vertex.aes$x            <- vertex.coords$x
#   vertex.aes$y            <- vertex.coords$y
#   
#   
#   if(identical(edges, TRUE)){
#     
#     edge.coords             <- edge.coord(graph, layout)
#     edge.l                  <- list(color=edge.color, alpha=edge.alpha, size=edge.size, linetype=edge.line)
#     e.i                     <- unlist(lapply(edge.l, length)) == 1
#     edge.attributes         <- edge.l[e.i]
#     edge.attributes$lineend <- "butt"
#     edge.aes                <- edge.l[e.i==FALSE]
#     edge.aes$x              <- edge.coords$start.x
#     edge.aes$y              <- edge.coords$start.y
#     edge.aes$xend           <- edge.coords$slut.x
#     edge.aes$yend           <- edge.coords$slut.y
#     
#     if(identical(edge.order, FALSE) == FALSE){
#       edge.aes              <- as.list(as.data.frame(edge.aes)[order(edge.order),])
#     } 
#   }
#   
#   if(identical(midpoints, TRUE)){
#     midpoint.attributes         <- edge.attributes
#     midpoint.attributes$arrow   <- midpoint.arrow 
#     midpoint.aes                <- edge.aes
#     midpoint.aes$x              <- (edge.coords$start.x + edge.coords$slut.x) / 2
#     midpoint.aes$y              <- (edge.coords$start.y + edge.coords$slut.y) / 2
#     
# #     ax                          <- edge.coords$slut.x - midpoint.aes$x
# #     ay                          <- edge.coords$slut.y - midpoint.aes$y
# #     crazy                       <- (edge.coords$slut.y / 10000) * 0.001
# #     els                         <- edge.coords$slut.y < midpoint.aes$y
# 
# # Her finder bevæger vi os 1/l hen af vectoren imod slutpunktet. x1 kan så være midpunktet.
# # l = sqrt((x2 - x1)^2 + (y2 -y1)^2)
# # x3 = x1 + (1/l) * (x2 - x1)
# # y3 = y1 + (1/l) * (y2 - y1)
# 
#     L                            <- sqrt(((edge.coords$slut.x - midpoint.aes$x)^2) + ((edge.coords$slut.y - midpoint.aes$y)^2))
#     midpoint.aes$xend            <- midpoint.aes$x + (1/L) * (edge.coords$slut.x - midpoint.aes$x)
#     midpoint.aes$yend            <- midpoint.aes$y + (1/L) * (edge.coords$slut.y - midpoint.aes$y)
#     #midpoint.aes$xend           <- midpoint.aes$x + ((ax / ay) * crazy)
#     #midpoint.aes$yend           <- midpoint.aes$y + crazy
# #    midpoint.aes$yend[els]      <- midpoint.aes$y[els] - crazy
#     midpoint.aes$group          <- paste(midpoint.aes$x, midpoint.aes$y)
#     
#     }
#   
#   text.l                  <- list(size=text.size, color=text.colour, alpha=text.alpha, vjust=text.vjust, lineheight=1)
#   t.i                     <- unlist(lapply(text.l, length)) == 1
#   text.attributes         <- text.l[t.i]
#   text.aes                <- text.l[t.i==FALSE]
#   text.aes$x              <- vertex.coords$x
#   text.aes$y              <- vertex.coords$y
#   text.aes$label          <- rownames(vertex.coords)
#   
#   # Plot edges
#   p <- ggplot()
#   
#   if(identical(edges, TRUE)){
#     edge.attributes$mapping     <- do.call("aes", edge.aes)
#     p <- p + do.call("geom_segment", edge.attributes, quote=TRUE)
#   }
#   
#   # Plot midpoints
#   
#   if(identical(midpoints, TRUE)){
#     midpoint.attributes$mapping     <- do.call("aes", midpoint.aes)
#     p <- p + do.call("geom_segment", midpoint.attributes, quote=TRUE)
#   }
#   
#   # Plot vertices
#   vertex.attributes$mapping     <- do.call("aes", vertex.aes)
#   p <- p + do.call("geom_point", vertex.attributes, quote=TRUE)
#   
#   # Plot text
#   if(text==TRUE){
#     text.attributes$mapping     <- do.call("aes", text.aes)
#     p <- p + do.call("geom_text", text.attributes, quote=TRUE)
#   }
#   
#   # Formatting
#   p <- p + theme_bw()
#   p <- p + labs(alpha="Alpha", shape="Shape", color="Color", linetype="Linetype", size="Size", fill="Fill")
#   
#   if(legend == "bottom")  p <- p + theme(legend.position = "bottom", legend.direction = "horizontal", legend.box = "horizontal")
#   if(legend == "none")    p <- p + theme(legend.position = "none")
#   
#   p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
#             axis.text.y=element_blank(),axis.ticks=element_blank(),
#             axis.title.x=element_blank(),
#             axis.title.y=element_blank(),
#             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#             panel.grid.minor=element_blank(),plot.background=element_blank())
#   
#   
#   
# }
# 


##################################
# Data object for plotting
# Det her kan optimeres ved at lave en lang factor, ændre levels, som nu indeholder både afsender og modtager og derefter dele den i 2 igen.
# Måske kan det også klares med merge()

edge.coord <- function(graph, layout){
  
  graph.names       <- V(graph)$name
  el                <- data.frame(moneca_get_edgelist(graph))
  
  el.X1.levels.x    <- levels(el$X1)
  el.X1.levels.y    <- levels(el$X1)
  el.X2.levels.x    <- levels(el$X2)
  el.X2.levels.y    <- levels(el$X2)
  
  for (i in 1:length(graph.names)){
    navn             <- graph.names[i]
    navn.el.pos.1    <- which(el.X1.levels.x == navn)
    navn.el.pos.2    <- which(el.X2.levels.x == navn)
    el.X1.levels.x[navn.el.pos.1]      <- layout[i, 1] 
    el.X1.levels.y[navn.el.pos.1]      <- layout[i, 2] 
    el.X2.levels.x[navn.el.pos.2]      <- layout[i, 1] 
    el.X2.levels.y[navn.el.pos.2]      <- layout[i, 2] 
  }
  
  out                   <- data.frame(start.x = el$X1, start.y = el$X1, slut.x = el$X2, slut.y = el$X2, weight = E(graph)$weight)
  levels(out$start.x)   <- el.X1.levels.x
  levels(out$start.y)   <- el.X1.levels.y
  levels(out$slut.x)    <- el.X2.levels.x
  levels(out$slut.y)    <- el.X2.levels.y
  
  out                   <- apply(out, 2, as.character)
  out                   <- apply(out, 2, as.numeric)
  
  as.data.frame(out)
}

vertex.coord <- function(graph, layout=layout.fruchterman.reingold(graph)){
  rownames(layout)  <- V(graph)$name
  layout            <- as.data.frame(layout, rownames(layout))
  colnames(layout)  <- c("x", "y")
  layout
}

#' Legacy Ego Network Visualization
#'
#' Creates ego network plots showing mobility patterns from a single focal position
#' using the legacy ggplot2 plotting system. For modern ego network analysis,
#' use \code{\link{plot_ego_ggraph}}.
#'
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param mxa.b The original mobility matrix used in the MONECA analysis.
#' @param id Integer or character specifying the focal node (ego) for the analysis.
#' @param lay Layout matrix for node positioning, typically from \code{\link{layout.matrix}}.
#' @param edge.size Numeric value for edge thickness. Default is 0.8.
#' @param border.padding Numeric value for segment boundary padding. Default is 1.
#' @param title.line Logical indicating whether to add a title line. Default is TRUE.
#' @param vertex.size Specification for vertex sizes. If "totals", sizes are derived
#'   from row/column totals in the mobility matrix. Otherwise, uses the specified values.
#' @param small.cell.reduction Numeric threshold below which vertices receive different
#'   shapes to indicate low mobility volumes. Default is 5.
#' @param edge.weight Character string specifying edge weight display. "discrete"
#'   creates categorical edge weights, otherwise uses continuous weights.
#' @param color.scheme Character string specifying the RColorBrewer color scheme
#'   for the visualization. Default is "RdPu".
#' @param ... Additional arguments passed to \code{\link{gg.moneca}}.
#'
#' @return A ggplot2 object showing the ego network.
#'
#' @details
#' This function creates a focused view of mobility patterns from a single position
#' in the social structure. It highlights both incoming and outgoing mobility flows
#' and uses different visual elements to represent:
#' \itemize{
#'   \item Edge colors/weights for relative risk levels
#'   \item Node sizes for total mobility volumes
#'   \item Node shapes for positions with low mobility
#'   \item Node colors for mobility share proportions
#' }
#' 
#' \strong{Note}: This function is maintained for backward compatibility.
#' For new analyses, consider using \code{\link{plot_ego_ggraph}} which offers
#' better performance and more modern styling options.
#'
#' @examples
#' \dontrun{
#' # Requires legacy data and eliter package
#' data(occupations)
#' ego.plot(mob.seg, mob.mat, id = 2)
#' 
#' # Customized ego plot
#' ego.plot(mob.seg, mob.mat, 
#'          id = 3,
#'          edge.size = 1.2,
#'          color.scheme = "Blues",
#'          small.cell.reduction = 10)
#' }
#' 
#' @seealso 
#' \code{\link{plot_ego_ggraph}} for modern ego network visualization,
#' \code{\link{gg.moneca}} for the underlying plotting function
#' 
#' @export

ego.plot <- function(segments, mxa.b, id = 1,
                     lay         = layout.matrix(segments),
                     edge.size   = 0.8,
                     border.padding = 1,
                     title.line  = TRUE,
                     vertex.size = "totals",
                     small.cell.reduction = 5,
                     edge.weight = "discrete",
                     color.scheme = "RdPu",
                     ...                   
){
  l             <- nrow(mxa.b)
  wm            <- weight.matrix(mxa.b, cut.off = 0, small.cell.reduction = small.cell.reduction, symmetric = FALSE)
  freq.mat      <- mxa.b[-l, -l]
  wm            <- segment.edges(segments, cut.off = 0.95, segment.reduction = 1, small.cell.reduction = small.cell.reduction)
  
  MR            <- wm[id, ]
  MC            <- wm[, id]
  M             <- (freq.mat[id, ] + freq.mat[, id]) / 2
  
  
  nul           <- M < small.cell.reduction
  nul[id]       <- "X_elite"
  nul           <- as.factor(nul)
  M[id]         <- NA
  EM            <- matrix(0 , nrow = nrow(wm), ncol = ncol(wm))
  EM[,id]       <- MC
  EM[id,]       <- MR
  EM[EM <= 0.99] <- 0
  dimnames(EM)  <- dimnames(freq.mat)
  M.share       <- M/sum(M[-id])
  EM[nul == "TRUE",]     <- 0
  EM[, nul == "TRUE"]    <- 0
  
  if(identical(vertex.size, "totals")){
    vertex.size        <- (mxa.b[l, ] + mxa.b[, l]) / 2
    vertex.size        <- vertex.size[-l]
    #vertex.size[id]    <- NA
  }
  
  scales                   <- list()
  scales$fill              <- scale_fill_gradientn(colours = brewer.pal(5, color.scheme), na.value = "black", labels = percent, name = "Mobile")
  scales$size              <- scale_size_continuous(range = c(2, 8), na.value = 8, name = "Antal")
  scales$shape             <- scale_shape_manual(values = c(21, 4, -0x25C9), guide = "none")
  scales$guide_fill        <- guides(fill = guide_legend(override.aes = list(size = 5, shape = 21)))
  scales$guide_size        <- guides(size = guide_legend(override.aes = list(shape = 21)))
    
  if(identical(edge.weight, "discrete")){
    gra.edges              <- moneca_graph_from_adjacency(EM, mode = "directed", weighted = TRUE, diag = NULL)                     
    ew                     <- E(gra.edges)$weight
    edge.weight                         <- ew
    edge.weight[ew < 1.5]               <- "1-1.5"
    edge.weight[ew > 1.5 & ew < 2.5]    <- "1.5-2.5"
    edge.weight[ew > 2.5 & ew < 5]      <- "2.5-5"
    edge.weight[ew > 5   & ew < Inf]    <- "+5"
    edge.weight                         <- as.factor(edge.weight)
    #scales$alpha                        <- scale_alpha_discrete(range = c(0.5, 1))
    scales$color                        <- scale_color_manual(values = brewer.pal(nlevels(edge.weight)+1, color.scheme)[-1], name = "Relativ risiko")
  }else{
    edge.weight             <- "weight"
  }
  
  
  p.ego     <- gg.moneca(segments, layout = lay,
                        edges = EM, edge.size = edge.size, edge.color = edge.weight, edge.alpha = 1,
                        vertex.fill = M.share, vertex.size = vertex.size, vertex.shape = nul,
                        border.padding = 1, border.text.size = 3, 
                        show.text = TRUE, ...)
  p.ego     <- p.ego + scales
  
  if(identical(title.line, TRUE))  p.ego  <- p.ego + annotate("segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, color = "black", lwd = 1)
  p.ego + ggtitle(rownames(wm)[id]) 
}

#' Legacy Multi-Level Stair Plot
#' 
#' Creates a series of plots showing how segmentation evolves across hierarchical
#' levels using the legacy ggplot2 system. For modern stair plots, use
#' \code{\link{plot_stair_ggraph}}.
#' 
#' @param segments A MONECA object returned by \code{\link{moneca}}.
#' @param level Integer vector specifying which levels to include in the stair plot.
#' @param layout Layout matrix for consistent node positioning across plots.
#' @param edges Edge matrix or specification for network edges.
#' @param mode Character string specifying graph mode ("directed" or "undirected").
#' @param vertex.size Specification for vertex sizes ("total" or numeric).
#' @param vertex.alpha Numeric transparency for vertices (0-1).
#' @param vertex.color Color specification for vertex borders.
#' @param vertex.shape Numeric shape code for vertices.
#' @param show.edges Logical indicating whether to display edges.
#' @param edge.size Numeric size for edges.
#' @param edge.alpha Transparency for edges ("weight" or numeric).
#' @param edge.color Color specification for edges.
#' @param edge.line Line type for edges ("solid", "dashed", etc.).
#' @param show.text Logical indicating whether to show vertex labels.
#' @param text.size Numeric size for text labels.
#' @param text.color Color for text labels.
#' @param text.alpha Transparency for text labels.
#' @param text.vjust Vertical adjustment for text labels.
#' @param show.borders Logical indicating whether to show segment boundaries.
#' @param border.size Size for segment borders.
#' @param border.fill Fill color for segment boundaries.
#' @param border.color Color for segment border lines.
#' @param border.alpha Transparency for segment borders.
#' @param border.padding Padding around segment boundaries.
#' @param border.text Logical indicating whether to show segment labels.
#' @param border.labels Specification for segment labels.
#' @param border.text.size Size for segment labels.
#' @param border.text.color Color for segment labels.
#' @param border.text.vjust Vertical adjustment for segment labels.
#' @param border.text.hjust Horizontal adjustment for segment labels.
#' @param midpoints Logical indicating whether to show edge midpoints.
#' @param midpoint.arrow Logical indicating whether to show arrows at midpoints.
#' @param edge.text Logical indicating whether to show edge labels.
#' @param edge.text.size Size for edge labels.
#' @param edge.text.alpha Transparency for edge labels.
#' @param legend Position specification for legend.
#' @param level.title Specification for level titles.
#' 
#' @return A list of ggplot2 objects, one for each segmentation level.
#' 
#' @details
#' This function creates multiple plots showing the progression of segmentation
#' across hierarchical levels. Each plot uses the same layout to maintain
#' consistency, making it easy to see how segments merge or split across levels.
#' 
#' \strong{Note}: This function is maintained for backward compatibility and
#' requires the eliter package. For new analyses, use \code{\link{plot_stair_ggraph}}
#' which offers better performance and modern styling.
#' 
#' @examples
#' \dontrun{
#' # Requires legacy data and eliter package
#' data(occupations)
#' plots <- stair.plot(mob.seg)
#' plots[[2]]  # Display second level
#' 
#' # Customized stair plot
#' custom_stairs <- stair.plot(mob.seg,
#'                            level = c(2, 3, 4),
#'                            vertex.size = "total",
#'                            show.borders = TRUE,
#'                            border.text.size = 5)
#' }
#' 
#' @seealso 
#' \code{\link{plot_stair_ggraph}} for modern stair plots,
#' \code{\link{gg.moneca}} for the underlying plotting function
#' 
#' @export
stair.plot              <- function(segments,
                                    level             = seq(segments$segment.list),
                                    layout             = layout.matrix(segments),
                                    edges              = segment.edges(segments, cut.off = 1, method = "all", segment.reduction = 0, level = 1),
                                    mode               = "directed",
                                    
                                    #membership         = segment.membership(segments),
                                    
                                    vertex.size        = "total",
                                    vertex.alpha       = 1,
                                    vertex.color       = "black",
                                    vertex.shape       = 21,
                                    
                                    show.edges         = TRUE,            
                                    edge.size          = 0.5,
                                    edge.alpha         = "weight",
                                    edge.color         = "black",
                                    edge.line          = "solid",
                                    
                                    show.text          = FALSE,
                                    text.size          = 3,
                                    text.color        = "black",
                                    text.alpha         = 1,
                                    text.vjust         = 1.5,
                                    
                                    show.borders       = TRUE,
                                    border.size        = 1,
                                    border.fill        = NA,
                                    border.color       = "black",
                                    border.alpha       = 1,
                                    border.padding     = 1,
                                    border.text        = TRUE,
                                    border.labels      = "segments",
                                    border.text.size   = 4,
                                    border.text.color  = "black",
                                    border.text.vjust  = -0.2,
                                    border.text.hjust  = 1,
                                    
                                    midpoints          = TRUE,
                                    midpoint.arrow     = arrow(angle = 20, length = unit(0.33, "cm"), ends = "last", type = "closed"),
                                    
                                    edge.text          = FALSE,
                                    edge.text.size     = 3,
                                    edge.text.alpha    = 0.9,
                                    
                                    legend             = "side",
                                    level.title        = "Level"
                                    ){  
  plot.arguments         <- as.list(environment())
  plot.arguments$level.title <- NULL
  
  list.scales             <- list()
  list.scales$size        <- scale_size_continuous(range = c(2, 4.5), guide = "none")
  list.scales$fill        <- scale_fill_grey(start = 0, end = 1, guide = "none")
  list.scales$alpha       <- scale_alpha_continuous(guide = "none", range = c(0.05, 0.9))
  
  level.list             <- lapply(level, FUN = seq, from = level[1])
  
  memberships.list        <- lapply(level.list, segment.membership, segments = segments)
  memberships.list        <- lapply(memberships.list, subset, select = 2)
  memberships.list        <- lapply(memberships.list, unlist, recursive = FALSE, use.names = FALSE)
  memberships.list        <- lapply(memberships.list, as.character)
  
  transition              <- list()
  transition[[1]]         <- rep(TRUE, length = length(memberships.list[[1]]))
  for(i in seq_along(level.list)[-1]) transition[[i]] <- memberships.list[[i-1]] == memberships.list[[i]]
  
  plot.list               <- list()
  for(i in seq_along(level.list)){
    plot.call             <- plot.arguments
    plot.call$segments   <- segments
    plot.call$vertex.fill <- transition[[i]]
    plot.call$level      <- level.list[[i]]
    p              <- do.call(what = gg.moneca, args = plot.call)
    title          <- paste(toOrdinal(level[i]), level.title)
    p              <- p + list.scales + ggtitle(title) + annotate("segment", x = Inf, xend = -Inf, y = Inf, yend = Inf, color = "black", lwd = 1)
    plot.list[[i]] <- p
  }
  plot.list
}

# Eksempler
# 
# class.mem       <- segment.membership(segments)$membership
# class.total     <- data.frame(segments$mat.list[[1]])$Total
# class.total     <- class.total[-length(class.total)]
# 
# gg.moneca(segments)
# 
# gg.moneca(segments, show.text = FALSE, legend = "bottom")
# 
# gg.moneca(segments, vertex.fill = class.mem, vertex.size = class.total)
# 
# edges.trunk <- edges
# 
# edges.trunk[edges >= 5] <- 5
# 
# gg.moneca(segments, edges = edges.trunk)

# Løsningen var at tage den convekse hull af punkterne - tegne cirkler for hvor hvert punkt i hullen og derefter tage hullen af disse cirkler.

