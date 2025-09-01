# Data generation

# library(moneca)
# mob.mat      <- read.csv("~/Dropbox/DINECA/Data/Moneca.csv", row.names = 1, header = TRUE, sep = ';', fileEncoding  ="UTF-8", check.names = FALSE)
# mob.mat      <- as.matrix(mob.mat)
# l            <- ncol(mob.mat)
# label        <- strtrim(rownames(mob.mat), 40)
# label.kode   <- read.csv("~/Dropbox/DINECA/Data/Oversat Moneca kategorier.csv", sep = ";", fileEncoding = "latin1")
# label        <- paste(label.kode$DISCO, colnames(mob.mat), sep = ": ")
# dimnames(mob.mat) <- list(label, label)
# mob.seg      <- segments <- moneca(mob.mat, segment.levels = 3)
# save(mob.mat, mob.seg, file = "~/MONECA/data/occupations.rda")

#' Generate Example Mobility Data
#' 
#' This function creates synthetic mobility data for examples and testing.
#' Use \code{\link{generate_mobility_data}} or \code{\link{generate_example_datasets}}
#' to create mobility matrices for analysis.
#' 
#' @name mobility_data_examples
#' @examples
#' # Generate basic synthetic data
#' mobility_data <- generate_mobility_data(n_classes = 5, seed = 123)
#' seg <- moneca(mobility_data, segment.levels = 2)
#' 
#' # Generate predefined examples
#' examples <- generate_example_datasets()
#' simple_seg <- moneca(examples$simple, segment.levels = 2)
#' 
NULL
