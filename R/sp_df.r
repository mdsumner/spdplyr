#' Store Spatial* objects in a data frame
#' 
#' @param x Spatial object
#' @param ... arguments passed to methods
#' @return nested table with Geometry column
#' @export
#'
#' @examples
#' sp <- spFromTable(semap, attr_tab = seatt, crs = "+proj=longlat +ellps=WGS84")
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

## we need this so that filter etc can work

# `[[.SpatialPolygons` <- function(x, i, j, ..., exact = TRUE) {
#   x[i]
# }
# 
#' @export
setMethod("[[", c("SpatialPolygons", "ANY", "missing"),
          function(x, i, j, ...) {
            if (!("data" %in% slotNames(x)))
              #stop("no [[ method for object without attributes")
            #x@data[[i]]
              x[i]
          }
)

#' @export
setMethod("[[", c("SpatialLines", "ANY", "missing"),
          function(x, i, j, ...) {
            if (!("data" %in% slotNames(x)))
              #stop("no [[ method for object without attributes")
              #x@data[[i]]
              x[i]
          }
)

#' @export
sp_df.Spatial <- function(x, ...) {
  tab <- has_data_frame(x)
  g <- geometry(x)
  #if (inherits(g, "SpatialPolygons")) glist <- x@polygons
  #if (inherits(g, "SpatialLines")) glist <- x@lines
 # class(glist) <- "Spatial_"
  tab$Spatial_ <- g
  #class(tab) <- c("sp_df", class(tab))
  tab
}


#' @export
format.SpatialPolygons <- function(x, ...) {
  #switch(class(geometry(x)), 
         vapply(x@polygons, obj_str.Polygons, character(1))
         #SpatialLines = vapply(x@lines, obj_str.Lines, character(1)))
}

#' @export
format.SpatialLines <- function(x, ...) {
  vapply(x@lines, obj_str.Lines, character(1))
}

# #' @export
# format.Spatial_ <- function(x, ...) {
#   switch(class(x[[1]]), 
#          Polygons =   vapply(x, obj_str.Polygons, character(1)), 
#          Lines = vapply(x, obj_str.Lines, character(1)))
# }

# @export
#format.Polygons <- function(x, ...) {
#  obj_str.Polygons(x)
#}
# @export
#format.Lines <- function(x, ...) {
#  obj_str.Lines(x)
#}

#obj_str <- function(x) UseMethod("obj_str")
obj_str.Lines <- function(x) sprintf("%s[%i]", class(x), length(x@Lines))
obj_str.Polygons <- function(x) sprintf("%s[%i]", class(x), length(x@Polygons))

