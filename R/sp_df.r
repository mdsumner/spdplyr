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

# explode_ <- function(x) {
#   l <- vector('list', length(x))
#   for (i in seq_along(l)) l[[i]] <- x[i]
#   l
# }
#' @export
sp_df.Spatial <- function(x, ...) {
  tab <- has_data_frame(x)
  tab$Spatial_ <- geometry(x)
  class(tab) <- c("sp_df", class(tab))
  tab
}

#' @export
`[.sp_df` <- function(x, i, j, ...) {
  ## this cannot work if i has duplicates (can't find plot order if polygons are replicated)
  ## though it does work for Spatial*DataFrame
  d <- NextMethod(x)
  class(d) <- c("sp_df", class(d))
  d
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



# #' @export
# #' @rdname sp_df
# #' @rawNamespace S3method(plot,sp_df)
# plot.sp_df <- function(x, ...) {
#   px <- as.data.frame(x)
#   plot(spFromTable(x[["Spatial_"]], as.data.frame(x)), ...)
# }

as.data.frame.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  NextMethod("as.data.frame", x)
}




names.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("names", x)
}
dim.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("dim", x)
}
ncol.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("ncol", x)
}
as_Spatial.sp_df <- function(x, ...) {
  df <- as.data.frame(x)
  sp <- x[["Spatial"]]
  .detectSpatial(class(sp))(sp, df, proj4string = CRS(attr(x, "crs")), match.ID = FALSE)
   
}

#' @importFrom sp SpatialMultiPointsDataFrame
.detectSpatial <- function(x) {
  switch(x, 
         SpatialPoints = SpatialPointsDataFrame,
         SpatialMultipoints = SpatialMultiPointsDataFrame,
         SpatialLines = SpatialLinesDataFrame, 
         SpatialPolygons = SpatialPolygonsDataFrame)
}
