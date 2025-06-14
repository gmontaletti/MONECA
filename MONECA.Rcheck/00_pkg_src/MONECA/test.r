# remove.packages("MONECA"); renv::install("MONECA_0.1.13.tar.gz", prompt = FALSE)

library(MONECA)
library(eliter)
packageVersion("MONECA")

# data("occupations")
g <- readRDS("../centrality/data/g_dataframe_diag.rds")
mob.mat <- addmargins(as_adjacency_matrix(g, attr = "weight", sparse = F ))

seg <- moneca(mob.mat
              , segment.levels = 3
              , mode = "Mutual"
              , small.cell.reduction = 0
              )
seg
membri <- MONECA::segment.membership(seg)
membri <- segment.membership.enhanced(seg, naming_strategy = "auto")

plot_moneca_ggraph(seg)
seg

# esempi <- MONECA::generate_example_datasets()

# seg <- moneca(esempi$complex)
plot_moneca_ggraph(seg)
plot_stair_ggraph(seg)

V(g)$name

plot_ego_ggraph(segments = seg
                , mobility_matrix = mob.mat
                , ego_id = "General business managers"
                , min_weight = 30
                , layout = "fr"
                , highlight_color = "firebrick",
                , flow_color = "viridis"
                )

MONECA::first.level.summary(seg)
MONECA::layout.matrix(seg)
MONECA::segment.colors(seg)
MONECA::segment.edges(seg)
MONECA::segment.edges(seg, cut.off = 1, method = "all", segment.reduction = 0, level = 1)

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
