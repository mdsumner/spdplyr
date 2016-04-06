#' Convert from Spatial*DataFrame to table.
#'
#' Input can be a \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}} or a \code{\link[sp]{SpatialPointsDataFrame}}.
#' @param x \code{\link[sp]{Spatial}} object
#' @param ... ignored
#'
#' @return \code{\link[dplyr]{tbl_df}} data_frame with columns
#' \itemize{
#'  \item SpatialPolygonsDataFrame "object" "part"   "branch"   "hole"   "x"      "y"
#'  \item SpatialLinesDataFrame "object" "part"   "branch"   "x"      "y"
#'  \item SpatialPointsDataFrame  "branch"   "object" "x"      "y"
#' }
#' @export
#'
sptable <- function(x, ...) {
  UseMethod("sptable")
}

#' @export
#' @rdname sptable
sptable.SpatialPolygonsDataFrame <- function(x, ...) {
  mat2d_f(.gobbleGeom(x, ...))
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
#'
#' @examples
"sptable<-" <-
  function(object, value) {
       spFromTable(value, proj4string(object), as.data.frame(object))

  }

#' Convert from dplyr tbl form to Spatial*DataFrame.
#'
#' @param x data_frame as created by \code{\link{sptable}}
#' @param crs projection, defaults to \code{NA_character_}
#' @param attr remaining data from the attributes
#'
#' @return Spatial*
#' @export
#' @importFrom dplyr %>% distinct_ as_data_frame
#' @importFrom sp coordinates CRS SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines SpatialLinesDataFrame Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
spFromTable <- function(x, crs, attr = NULL, ..., quiet = FALSE) {
  if (missing(crs)) crs <- NA_character_
  ## raster::geom form
  target <- detectSpClass(x)
  dat <- x %>% distinct_("object") %>% as.data.frame
  
  dat <- dat[, -match(names(dat), geomnames()[[target]])]
  
  n_object <- length(unique(x$object))
  n_attribute <- nrow(attr)
  if (is.null(n_attribute)) n_attribute <- n_object
  if (!n_object == n_attribute){
    
    spFromTable(value, proj4string(object), as.data.frame(object))
  } else {
    if (!quiet) warning("modifications removed the relation between object and data, using a dummy data frame of attributes")
    attr <- data.frame(id = seq(n_object))
  }
  
  ## this is rough and ready, needs proper matching checks
  dat <- cbind(dat, attr)  
  gom <- switch(target,
         SpatialPolygonsDataFrame = reverse_geomPoly(x, dat, crs),
         SpatialLinesDataFrame = reverse_geomLine(x, dat, crs),
         SpatialPointsDataFrame = reverse_geomPoint(x, dat, crs)
         )
 gom
}

reverse_geomPoly <- function(x, d, proj) {
  objects <- split(x, x$object)
  ## match.ID should be replaced by method to carry the original rownames somehow
  SpatialPolygonsDataFrame(SpatialPolygons(lapply(objects, loopPartsPoly), proj4string = CRS(proj)), d, match.ID = FALSE)
}
loopPartsPoly <- function(a) Polygons(lapply(split(a, a$part), function(b) Polygon(as.matrix(b[, c("x", "y")]), hole = b$hole[1L] == 1)), as.character(a$object[1L]))


reverse_geomLine <- function(x, d, proj) {
  objects <- split(x, x$object)
  SpatialLinesDataFrame(SpatialLines(lapply(objects, loopPartsLine), proj4string = CRS(proj)), d)
}
loopPartsLine<- function(a) Lines(lapply(split(a, a$part), function(b) Polygon(as.matrix(b[, c("x", "y")]))), as.character(a$object[1L]))

reverse_geomPoint <- function(a, d, proj) {
 # stop("not implemented")
  ## the decomposition is not yet applied for Multipoints . . .
 ## if (length(unique(a$object)) > 1) warning("no support for Multipoints yet")
 SpatialPointsDataFrame(SpatialPoints(as.matrix(a[, c("x", "y")])), d, proj4string = CRS(proj))
}

detectSpClass <- function(x) {
  #names(sptable(wrld_simpl))
  #[1] "object" "part"   "branch"   "hole"   "x"      "y"
  #names(sptable(as(wrld_simpl, "SpatialLinesDataFrame")))
  #"object" "part"   "branch"   "x"      "y"
  #names(sptable(as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")))
  # "branch"   "object" "x"      "y"
  gn <-geomnames()
  if (all(gn$SpatialPolygonsDataFrame %in% names(x))) return("SpatialPolygonsDataFrame")
  if (all(gn$SpatialLinesDataFrame %in% names(x))) return("SpatialLinesDataFrame")
  if (all(gn$SpatialPointsDataFrame %in% names(x))) return("SpatialPointsDataFrame")
  stop('cannot create Spatial* object from this input')

}

geomnames <- function() {
  list(SpatialPolygonsDataFrame = c("object", "part", "branch", "hole", "x", "y"),
       SpatialLinesDataFrame = c("object", "part", "branch", "x", "y"),
       SpatialPointsDataFrame = c("branch", "object", "x", "y"))
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
                     lst <- list(part = rep(j, nr), 
                                 branch = rep(j + cnt, nr), 
                                 hole = rep(.holes(x, i, j, typ), nr), 
                                 order = seq(nr),
                                 x = coords[,1], 
                                 y = coords[,2])
                     as_data_frame(lst[!sapply(lst, is.null)])
                   }
      )
      psd <- do.call(bind_rows, ps)
      objlist[[i]] <- bind_cols(data_frame(object = rep(i, nrow(psd))), psd)
      cnt <- cnt + nsubobs
    }
  obs <- do.call(bind_rows, objlist)
  
  rownames(obs) <- NULL

 
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
  colnames(xy) <- c('branch', 'object', 'x', 'y')
  return(xy)
}
