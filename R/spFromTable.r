## spFromTable must deal with the following cases (so we don't need class-methods)
## fortified table (done)
## sp_df type, easy just grab the "Spatial_" column
## sportify type, unnest once
## nsp_df type, unnest twice (need name of column)
## db_df type, inner_join Object, Branch, Vertex and re-nest to sportify


#' Convert from dplyr tbl form to Spatial*DataFrame.
#'
#' @param x data_frame as created by \code{\link{sptable}}
#' @param crs projection, defaults to \code{NA_character_}
#' @param attr_tab remaining data from the attributes
#' @param ... unused
#' @return Spatial*
#' @export
#' @importFrom dplyr %>% distinct_ as_data_frame
#' @importFrom sp coordinates CRS SpatialPoints SpatialPointsDataFrame Line Lines SpatialLines SpatialLinesDataFrame Polygon Polygons SpatialPolygons SpatialPolygonsDataFrame
#' @examples 
#' semap1 <- semap  %>% dplyr::filter(y_ > -89.9999)
#' sp <- spFromTable(semap1, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
#' ## look, seamless Antarctica!
#' ## library(rgdal); plot(spTransform(sp, "+proj=laea +lat_0=-70"))
spFromTable <- function(x, attr_tab = NULL, crs, ...) {
  if (missing(crs)) crs <- attr(x, "crs")
  if (is.null(crs)) crs <- NA_character_
  ## raster::geom form
  target <- detectSpClass(x)
  dat <- x %>% distinct_("object_")
  
   n_object <- length(unique(x$object_))
   n_attribute <- nrow(attr_tab)
   if (is.null(n_attribute)) n_attribute <- n_object
  if (!(n_attribute == n_object)) stop("number of rows in attr must match distinct object in x") 
  if (!is.null(attr_tab)) dat <- bind_cols(dat, attr_tab)
  dat <- as.data.frame(dat)
  gom <- switch(target,
                SpatialPolygonsDataFrame = reverse_geomPoly(x, dat, crs),
                SpatialLinesDataFrame = reverse_geomLine(x, dat, crs),
                SpatialPointsDataFrame = reverse_geomPoint(x, dat, crs)
  )
  gom
}



## convert this to sptable type (Spatial_, Object)
geomnames <- function() {
  list(SpatialPolygonsDataFrame = c("object_",  "branch_", "island_",  "x_", "y_"),
       SpatialLinesDataFrame = c("object_",  "branch_",  "x_", "y_"),
       SpatialPointsDataFrame = c("branch_", "object_", "x_", "y_"))
}


reverse_geomPoly <- function(x, d, proj) {
  objects <- split(x, x$object_)
  ## remove those columns used by reconstruction?
  d$branch_ <- d$object_ <- d$island_ <- d$order_ <- d$x_ <- d$y_ <- NULL
  if (ncol(d) < 1L) d$id_ <- seq(nrow(d))  ## we might end up with no attributes
  ## match.ID should be replaced by method to carry the original rownames somehow
  SpatialPolygonsDataFrame(SpatialPolygons(lapply(objects, loopBranchPoly), proj4string = CRS(proj)), d, match.ID = FALSE)
}
loopBranchPoly <- function(a) Polygons(lapply(split(a, a$branch), function(b) Polygon(as.matrix(b[, c("x_", "y_")]), hole = !b$island_[1L] == 1)), as.character(a$object_[1L]))


reverse_geomLine <- function(x, d, proj) {
  objects <- split(x, x$object_)
  d$branch_ <- d$object_ <- d$order_ <- d$x_ <- d$y_ <- NULL
  if (ncol(d) < 1L) d$id_ <- seq(nrow(d))  ## we might end up with no attributes
  SpatialLinesDataFrame(SpatialLines(lapply(objects, loopBranchLine), proj4string = CRS(proj)), d)
}
loopBranchLine<- function(a) Lines(lapply(split(a, a$branch), function(b) Polygon(as.matrix(b[, c("x_", "y_")]))), as.character(a$object_[1L]))

reverse_geomPoint <- function(a, d, proj) {
  # stop("not implemented")
  ## the decomposition is not yet applied for Multipoints . . .
  ## if (length(unique(a$object)) > 1) warning("no support for Multipoints yet")
  SpatialPointsDataFrame(SpatialPoints(as.matrix(a[, c("x_", "y_")])), d, proj4string = CRS(proj))
}

detectSpClass <- function(x) {
  gn <-geomnames()
  if (all(gn$SpatialPolygonsDataFrame %in% names(x))) return("SpatialPolygonsDataFrame")
  if (all(gn$SpatialLinesDataFrame %in% names(x))) return("SpatialLinesDataFrame")
  if (all(gn$SpatialPointsDataFrame %in% names(x))) return("SpatialPointsDataFrame")
  stop('cannot create Spatial* object from this input')
  
}