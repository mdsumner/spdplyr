cna <- function(x, na = NA_real_) {
  c(x, na)
}

#' Convert Spatial Polygons data frame to gggeom format. 
#' 
#' This can work, but it does not fit the one-branch-per-object assumtion in gggeom:::plot.geom_polygon. 
#'
#' @param x \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @param ... ignored
#'
#' @return data frame with recursive x_ and y_ ggeom::coords. 
#' @export
as_gggeom <- function(x,  ...) {
  data <- as.data.frame(x)
  x <- sptable(x)
  data$y_ <- data$x_ <- vector("list", nrow(data))
  for (i in unique(x$object_)) {
    asub <- x[x$object_ == i, c("branch_", "x_", "y_")]
    data$x_[[i]] <- unname(head(unlist(lapply(split(asub$x, asub$branch_), cna)), -1))
    data$y_[[i]] <- unname(head(unlist(lapply(split(asub$y, asub$branch_), cna)), -1))
  } 
  class(data$x_) <- "coords"
  class(data$y_) <- "coords"
  
  class(data) <- c("geom_polygon", "geom_path", "geom", "data.frame")
  data
}

