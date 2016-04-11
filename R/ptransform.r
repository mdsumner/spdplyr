

#' Map projection for sptable
#'
#' @param x object
#' @param target proj4 string
#' @param ... passed to methods
#'
#' @return  object
#' @export
#' @importFrom proj4 ptransform
#' @importFrom raster projection projection<-
#' @examples
ptransform <- function(x, ...) {
  UseMethod("ptransform")
}

#' @importFrom raster projection
#' @rdname ptransform
#' @export
ptransform.Spatial <- function(x, target, ...) {
  y <- sptable(x) 
  m <- y %>% select_("x", "y") %>% as.matrix() ##mutate(x = x * pi/180, y = y * pi/180)
  m <- m * pi/180
  src <- raster::projection(x)
  m <- proj4::ptransform(m, src.proj = src, dst.proj = target, ...)
  y$x <- m[,1]
  y$y <- m[,2]
  sptable(x) <- y
  projection(x) <- target
  x
}
ptransform.default <- function(x, ...) {
  proj4::ptransform(data = x, ...)
}
