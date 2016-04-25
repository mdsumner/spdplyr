#' Convert from Spatial*DataFrame to table.
#'
#' Decompose a Spatial object to a single table structured as a row for every coordinate in all the sub-geometries, including duplicated coordinates that close polygonal rings, close lines and shared vertices between objects. 
#' 
#' Input can be a \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}} or a \code{\link[sp]{SpatialPointsDataFrame}}.
#' @param x \code{\link[sp]{Spatial}} object
#' @param ... ignored
#'
#' @return \code{\link[dplyr]{tbl_df}} data_frame with columns
#' \itemize{
#'  \item SpatialPolygonsDataFrame "object_"   "branch_"   "island_"   "_"      "y_"
#'  \item SpatialLinesDataFrame "object_"   "branch_"   "x_"      "y_"
#'  \item SpatialPointsDataFrame  "branch_"   "object_" "x_"      "y_"
#' }
#' @export
#'
sptable <- function(x, ...) {
  UseMethod("sptable")
}

#' @export
#' @rdname sptable
sptable.SpatialPolygonsDataFrame <- function(x, ...) {
  .gobbleGeom(x, ...)
}

#' @export
#' @rdname sptable
sptable.SpatialLinesDataFrame <- function(x, ...) {
  mat2d_f(.gobbleGeom(x, ...))
}

#' @export
#' @rdname sptable
#' @importFrom dplyr bind_cols
sptable.SpatialPointsDataFrame <- function(x, ...) {
  bind_cols(mat2d_f(.pointsGeom(x, ...)), x@data)
}

## TODO multipoints
#' @importFrom dplyr data_frame
mat2d_f <- function(x) {
  as_data_frame(as.data.frame((x)))
}


#' @rdname sptable
#' @param object Spatial object
#' @param value modified sptable version of object
#'
#' @return Spatial object
#' @export
"sptable<-" <-
  function(object, value) {
       spFromTable(value, as.data.frame(object), proj4string(object))

  }





.coordsIJ <- function(x, i, j, type) {
  switch(type, 
         line = x@lines[[i]]@Lines[[j]]@coords, 
         poly =  x@polygons[[i]]@Polygons[[j]]@coords)
}

.nsubobs <- function(x, i, type) {
  length(
    switch(type, 
         line = x@lines[[i]]@Lines, 
         poly = x@polygons[[i]]@Polygons)
)
}
.holes <- function(x, i, j, type) {
  switch(type, 
         line = NULL, 
         poly = x@polygons[[i]]@Polygons[[j]]@hole
         )
}
## adapted from raster package R/geom.R
## generalized on Polygon and Line
#' @importFrom sp geometry
#' @importFrom dplyr bind_rows
.gobbleGeom <-   function(x,  ...) {
  gx <- geometry(x)
  typ <- switch(class(gx), 
                SpatialPolygons = "poly", 
                SpatialLines = "line")
  nobs <- length(geometry(x))
  objlist <- vector("list", nobs)
  cnt <- 0L
  for (i in seq(nobs)) {
      nsubobs <- .nsubobs(x, i, typ) 
      ps <- lapply(1:nsubobs,
                   function(j) {
                     coords <- .coordsIJ(x, i, j, typ)
                     nr <- nrow(coords)
                     lst <- list(
                                 branch_ = rep(j + cnt, nr), 
                                 island_ = rep(!.holes(x, i, j, typ), nr), 
                                 order_ = seq(nr),
                                 x_ = coords[,1], 
                                 y_ = coords[,2])
                     as_data_frame(lst[!sapply(lst, is.null)])
                   }
      )
      psd <- do.call(bind_rows, ps)
      objlist[[i]] <- bind_cols(data_frame(object_ = rep(i, nrow(psd))), psd)
      cnt <- cnt + nsubobs
    }
  obs <- do.call(bind_rows, objlist)
  
  rownames(obs) <- NULL

  attr(obs, "crs") <- proj4string(x)
  return( obs )
}





.pointsGeom <-  function(x, ...) {
  xy <- coordinates(x)
  ##xy <- cbind(1:nrow(xy), xy)
  if (is.list(x@coords)) {
    br <- rep(seq_along(x@coords), unlist(lapply(x@coords, nrow)))

  } else {
    br <- seq(nrow(xy))
  }
  xy <- cbind(br, br, xy)
  colnames(xy) <- c('branch_', 'object_', 'x_', 'y_')
  return(xy)
}
