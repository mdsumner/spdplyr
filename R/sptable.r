#' Convert from Spatial*DataFrame to table.
#'
#' Input can be a \code{\link[sp]{SpatialPolygonsDataFrame}}, \code{\link[sp]{SpatialLinesDataFrame}} or a \code{\link[sp]{SpatialPointsDataFrame}}.
#' @param x \code{\link[sp]{Spatial}} object
#' @param ... ignored
#'
#' @return \code{\link[dplyr]{tbl_df}} data_frame with columns
#' \itemize{
#'  \item SpatialPolygonsDataFrame "object" "part"   "cump"   "hole"   "x"      "y"
#'  \item SpatialLinesDataFrame "object" "part"   "cump"   "x"      "y"
#'  \item SpatialPointsDataFrame  "cump"   "object" "x"      "y"
#' }
#' @export
#'
sptable <- function(x, ...) {
  UseMethod("sptable")
}

#' @export
#' @rdname sptable
sptable.SpatialPolygonsDataFrame <- function(x, ...) {
  mat2d_f(.polysGeom(x, ...))
}

#' @export
#' @rdname sptable
sptable.SpatialLinesDataFrame <- function(x, ...) {
  mat2d_f(.linesGeom(x, ...))
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
spFromTable <- function(x, crs, attr = NULL, ...) {
  if (missing(crs)) crs <- NA_character_
  ## raster::geom form
  target <- detectSpClass(x)
  dat <- x %>% distinct_("object") %>% as.data.frame
  dat <- dat[, -match(geomnames()[[target]], names(dat))]
  
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
  #[1] "object" "part"   "cump"   "hole"   "x"      "y"
  #names(sptable(as(wrld_simpl, "SpatialLinesDataFrame")))
  #"object" "part"   "cump"   "x"      "y"
  #names(sptable(as(as(wrld_simpl, "SpatialLinesDataFrame"), "SpatialPointsDataFrame")))
  # "cump"   "object" "x"      "y"
  gn <-geomnames()
  if (all(gn$SpatialPolygonsDataFrame %in% names(x))) return("SpatialPolygonsDataFrame")
  if (all(gn$SpatialLinesDataFrame %in% names(x))) return("SpatialLinesDataFrame")
  if (all(gn$SpatialPointsDataFrame %in% names(x))) return("SpatialPointsDataFrame")
  stop('cannot create Spatial* object from this input')

}

geomnames <- function() {
  list(SpatialPolygonsDataFrame = c("object", "part", "cump", "hole", "x", "y"),
       SpatialLinesDataFrame = c("object", "part", "cump", "x", "y"),
       SpatialPointsDataFrame = c("cump", "object", "x", "y"))
}

## adapted from raster package R/geom.R
.polysGeom <-   function(x,  ...) {

  nobs <- length(x@polygons)
  objlist <- list()
  cnt <- 0
  for (i in 1:nobs) {
      nsubobs <- length(x@polygons[[i]]@Polygons)
      ps <- lapply(1:nsubobs,
                   function(j)
                     cbind(j, j+cnt, x@polygons[[i]]@Polygons[[j]]@hole, x@polygons[[i]]@Polygons[[j]]@coords)
      )
      objlist[[i]] <- cbind(i, do.call(rbind, ps))
      cnt <- cnt+nsubobs
    }
  

  obs <- do.call(rbind, objlist)
  colnames(obs) <- c('object', 'part', 'cump', 'hole', 'x', 'y')
  rownames(obs) <- NULL

 
  return( obs )
}




.linesGeom <-  function(x,  ...) {

  nobs <- length(x@lines)
  objlist <- list()
  cnt <- 0
for (i in 1:nobs) {
      nsubobj <- length(x@lines[[i]]@Lines)
      ps <- lapply(1:nsubobj, function(j) cbind(j, j+cnt, x@lines[[i]]@Lines[[j]]@coords))
      objlist[[i]] <- cbind(i, do.call(rbind, ps))
      cnt <- cnt+nsubobj
    }
  
  obs <- do.call(rbind, objlist)
  colnames(obs) <- c('object', 'part', 'cump', 'x', 'y')
  rownames(obs) <- NULL

  return (obs)
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
  colnames(xy) <- c('cump', 'object', 'x', 'y')
  return(xy)
}
