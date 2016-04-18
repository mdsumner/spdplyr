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
#'
#' @examples
as_gggeom <- function(x,  ...) {
  data <- as.data.frame(x)
  x <- sptable(x)
  data$y_ <- data$x_ <- vector("list", nrow(data))
  for (i in unique(x$object)) {
    asub <- x[x$object == i, c("branch", "x", "y")]
    data$x_[[i]] <- unname(head(unlist(lapply(split(asub$x, asub$branch), cna)), -1))
    data$y_[[i]] <- unname(head(unlist(lapply(split(asub$y, asub$branch), cna)), -1))
  } 
  class(data$x_) <- "coords"
  class(data$y_) <- "coords"
  
  class(data) <- c("geom_polygon", "geom_path", "geom", "data.frame")
  data
}

