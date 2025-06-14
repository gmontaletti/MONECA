# remove.packages("MONECA")
# renv::install("MONECA_0.1.7.tar.gz")

library(MONECA)
library(eliter)
packageVersion("MONECA")

# data("occupations")
g <- read_rds("../centrality/data/g_dataframe_diag.rds")
mob.mat <- addmargins(as_adjacency_matrix(g, attr = "weight", sparse = F ))

seg <- moneca(mob.mat
              , segment.levels = 3
              , mode = "Mutual"
              , small.cell.reduction = 3
              )

plot_moneca_ggraph(seg)
seg

# esempi <- MONECA::generate_example_datasets()

# seg <- moneca(esempi$complex)
plot_moneca_ggraph(seg)
plot_stair_ggraph(seg)

V(g)$name

plot_ego_ggraph(segments = seg
                , mobility_matrix = mob.mat
                , ego_id = "Domestic services personnel"
                , min_weight = 130
                , layout = "fr"
                , highlight_color = "firebrick",
                , flow_color = "viridis"
                )

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
# fix in moneca.plot():Error in layout.matrix(segments) : segments$mat.list[[1]] is NULL

MONECA::gg.moneca(seg) 

gg.moneca(segments = seg
          , edges = segment.edges(seg, cut.off = 1
                                  , method = "all"
                                  , segment.reduction = 0
                                  , level = 1)
          )

seg
MONECA::layout.matrix(seg)
