#' Convert from dplyr tbl form to Spatial*DataFrame.
#'
#' @param x data_frame as created by \code{\link{sptable}}
#' @param crs projection, defaults to \code{NA_character_}
#' @param attr remaining data from the attributes
#' @param ... unused
#' @return Spatial*
#' @export
#' @importFrom dplyr %>% distinct_ as_data_frame
#' @importFrom sp coordinates CRS SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines SpatialLinesDataFrame Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @examples 
#' semap1 <- semap  %>% dplyr::filter(y > -89.9999)
#' sp <- spFromTable(semap1, attr = seatt, crs = "+proj=longlat +ellps=WGS84")
#' ## look, seamless Antarctica!
#' ## library(rgdal); plot(spTransform(sp, "+proj=laea +lat_0=-70"))
spFromTable <- function(x, crs, attr = NULL, ...) {
  if (missing(crs)) crs <- attr(x, "crs")
  if (is.null(crs)) crs <- NA_character_
  ## raster::geom form
  target <- detectSpClass(x)
  dat <- x %>% distinct_("object") %>% as.data.frame()
  
  n_object <- length(unique(x$object))
  n_attribute <- nrow(attr)
  if (is.null(n_attribute)) n_attribute <- n_object
  
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
  ## remove those columns used by reconstruction?
  d$part <- d$branch <- d$object <- d$hole <- d$order <- d$x <- d$y <- NULL
  ## match.ID should be replaced by method to carry the original rownames somehow
  SpatialPolygonsDataFrame(SpatialPolygons(lapply(objects, loopPartsPoly), proj4string = CRS(proj)), d, match.ID = FALSE)
}
loopPartsPoly <- function(a) Polygons(lapply(split(a, a$branch), function(b) Polygon(as.matrix(b[, c("x", "y")]), hole = b$hole[1L] == 1)), as.character(a$object[1L]))


reverse_geomLine <- function(x, d, proj) {
  objects <- split(x, x$object)
  d$part <- d$branch <- d$object <- d$order <- d$x <- d$y <- NULL
  SpatialLinesDataFrame(SpatialLines(lapply(objects, loopPartsLine), proj4string = CRS(proj)), d)
}
loopPartsLine<- function(a) Lines(lapply(split(a, a$branch), function(b) Polygon(as.matrix(b[, c("x", "y")]))), as.character(a$object[1L]))

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