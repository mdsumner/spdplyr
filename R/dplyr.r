#' Mutate for Spatial
#' 
#' mutate . . .
#' 
#' @export
#' @examples 
#' library(sp)
#' library(maptools)
#' data(wrld_simpl)
#' library(dplyr)
#' library(spbabel)   ## devtools::install_github("mdsumner/spbabel", ref = "pipe")
#' w2 <- wrld_simpl %>% mutate(NAME = "allthesame")
#' 
#' ## how to pipe a reprojection? some special formula syntax for ~x+y ?? ~lon+lat ??
mutate_.Spatial_DataFrame <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  pr4 <- proj4string(.data)
  dgeom <- sptable(.data)
  dat <- as.data.frame(.data)
  spFromTable(mutate_(dgeom, .dots = dots), crs = pr4, attr = dat)
}

#' @export
mutate_.SpatialPolygonsDataFrame <- mutate_.Spatial_DataFrame
#' @export
mutate_.SpatialLinesDataFrame <- mutate_.Spatial_DataFrame
#' @export
mutate_.SpatialPointsDataFrame <- mutate_.Spatial_DataFrame
#' @export
mutate_.SpatialMultiPointsDataFrame <- mutate_.Spatial_DataFrame