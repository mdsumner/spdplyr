#' sp_df to store the list of Polygons in a column
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' #x <- sp_df(wrld_simpl)
#' #as_Spatial.sp_df(d)
sp_df <- function(x) {
  tab <- as_data_frame(as.data.frame(x))
  tab$Spatial <- geometry(x)
  attr(tab, "crs") <- proj4string(x)
  class(tab) <- c("sp_df", class(tab))
  tab
}

names.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  names(x)
}
print.sp_df <- function(x, ...) {
  catclass <- class(x$Spatial)
  x$Spatial <- NULL
  
  cat(catclass, "\n")
  NextMethod("print", x)
}

as.data.frame.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  NextMethod("as.data.frame", x)
}

plot.sp_df <- function(x, ...) {
  px <- as.data.frame(x)
  plot(SpatialPolygonsDataFrame(x$Spatial, px, match.ID = FALSE), ...)
}

`[.sp_df` <- function(x, i, j, ...) {
  id <- setNames(seq(nrow(x)), rownames(x))[i]
  sp <- x[["Spatial"]]
  #class(x) <- setdiff(class(x), "sp_df")
  d <- NextMethod("[", x)
  d$Spatial <- sp[id]
  class(d) <- c("sp_df", class(d))
  d
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
