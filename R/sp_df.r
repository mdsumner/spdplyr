#' Store Spatial* objects in a data frame
#' 
#' @param x Spatial object
#'
#' @return nested table with Geometry column
#' @export
#'
#' @examples
#' spdf <- sp_df(wrld_simpl)
#' #as_Spatial.sp_df(spdf)
#' 
#' sdf <- sp_df(geometry(wrld_simpl))
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


# #' Retrieve geometry from sp_df
# #' @name geometry-methods
# #' @param x sp_df object
# #' @param ... ignored
# #' @seealso \code{\link[sp]{geometry-methods}}
# #' @export
# geometry <- function(x, ...) {
#   UseMethod("geometry")
# }
# @export
 .geometry.sp_df <- function(obj) obj[["Spatial_"]]
# 
 setOldClass("sp_df")
#' @export
 setMethod("geometry", "sp_df", .geometry.sp_df)
# #' @export
# sp_df.list <- function(x, ...) {
#   ## detect class and upgrade to Spatial?
#   ## ultimately I want these to have autononomy, geoms with crs and mixable with other topology so keep as a list
#   sp_df(SpatialPolygons(x))  ## need to detect class for this to work
# }

#' @export
print.sp_df <- function(x, ...) {
  catclass <- class(x$Spatial_)
  x$Spatial_ <- sprintf("<%s>", catclass)
  cat(catclass, "\n")
  NextMethod("print", x)
}

as.data.frame.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  NextMethod("as.data.frame", x)
}

#' @export
plot.sp_df <- function(x, ...) {
  px <- as.data.frame(x)
  plot(SpatialPolygonsDataFrame(x$Spatial, px, match.ID = FALSE), ...)
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
