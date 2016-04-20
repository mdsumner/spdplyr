#' Store Spatial* objects in a data frame
#' 
#' @param x Spatial object
#' @param ... arguments passed to methods
#' @return nested table with Geometry column
#' @export
#'
#' @examples
#' sp <- spFromTable(semap, attr = seatt, crs = "+proj=longlat +ellps=WGS84")
#' spdf <- sp_df(sp)
sp_df <- function(x, ...) {
 UseMethod("sp_df") 
}

has_data_frame <- function(x) {
  if (has_data(x)) {
    as_data_frame(as.data.frame(x))
  } else {
    data_frame(x = seq(length(x)))[, -1L]
  }
}


#' @export
sp_df.Spatial <- function(x, ...) {
  tab <- has_data_frame(x)
  tab$Spatial_ <- geometry(x)
  class(tab) <- c("sp_df", class(tab))
  tab
}

#' @export
format.Spatial <- function(x, ...) {
  switch(class(geometry(x)), 
         SpatialPolygons =   vapply(x@polygons, obj_str.Polygons, character(1)), 
         SpatialLines = vapply(x@lines, obj_str.Lines, character(1)))
}

#obj_str <- function(x) UseMethod("obj_str")
obj_str.Lines <- function(x) sprintf("%s[%i]", class(x), length(x@Lines))
obj_str.Polygons <- function(x) sprintf("%s[%i]", class(x), length(x@Polygons))


