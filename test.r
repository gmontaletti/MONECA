# install.packages("MONECA_0.1.4.tar.gz")
library(igraph)
library(MONECA)
library(eliter)
packageVersion("MONECA")

# data("occupations")
lombardia <- read_rds("../centrality/data/g_dataframe_diag.rds")
mob.mat <- as_adjacency_matrix(lombardia, attr = "weight", sparse = F )
mob.mat <- addmargins(mob.mat)

seg <- moneca(mob.mat, segment.levels = 3, mode = "Mutual", small.cell.reduction = 5)

plot_moneca_ggraph(seg)
seg

esempi <- MONECA::generate_example_datasets()

# seg <- moneca(esempi$complex)
plot_moneca_ggraph(seg)
plot_stair_ggraph(seg)

plot_ego_ggraph(segments = seg, mobility_matrix = mob.mat, ego_id = 33 )
MONECA::first.level.summary(seg)
MONECA::layout.matrix(seg)
MONECA::segment.colors(seg)
MONECA::segment.edges(seg)
MONECA::segment.edges(seg, cut.off = 1, method = "all", segment.reduction = 0, level = 1)
membri <- MONECA::segment.membership(seg)
squal <- MONECA::segment.quality(seg)
MONECA::stair.plot(seg)
vmob <-MONECA::vertex.mobility(seg)
MONECA::weight.matrix(mob.mat)

MONECA::moneca.plot(seg)
# fix in moneca.plot(): Error in -l : invalid argument to unary operator

MONECA::gg.moneca(seg) 

gg.moneca(segments = seg,
          edges = segment.edges(seg, cut.off = 1, method = "all", segment.reduction = 0, level = 1))

seg
MONECA::layout.matrix(seg)
