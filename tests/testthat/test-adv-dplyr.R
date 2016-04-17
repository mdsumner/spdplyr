context("adv-dplyr")


library(maptools)
library(dplyr)
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")
#mpoint1 <- SpatialMultiPointsDataFrame(lapply(split(line1, seq(nrow(line1))), function(y) coordinates(as(y, "SpatialPoints"))), 
#                                       as.data.frame(line1))

data(mpoint1)
poly1
