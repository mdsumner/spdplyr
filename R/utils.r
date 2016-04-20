has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}


has_data <- function(x) .hasSlot(x, "data")


#' @importFrom sp SpatialMultiPointsDataFrame
.detectSpatial <- function(x) {
  switch(x, 
         SpatialPoints = SpatialPointsDataFrame,
         SpatialMultipoints = SpatialMultiPointsDataFrame,
         SpatialLines = SpatialLinesDataFrame, 
         SpatialPolygons = SpatialPolygonsDataFrame)
}



#' @importFrom sp bbox proj4string 
#' @importFrom dplyr tbl_df group_by
#' @importFrom ggplot2 aes ggplot 
.print_Spatial <- 
function (x, ...) 
{
  cat("class       :", class(x), "\n")
  isRaster <- hasData <- FALSE
  nc <- 0
  if (has_data(x)) {
    hasData <- TRUE
    nc <- ncol(x@data)
  }
  ln <- 1
  if (inherits(x, "SpatialPixels")) {
    isRaster <- TRUE
    cr <- x@grid@cells.dim
    cat("dimensions  : ", cr[2], ", ", cr[1], ", ", nrow(x@coords), 
        ", ", nc, "  (nrow, ncol, npixels, nlayers)\n", sep = "")
    cs <- x@grid@cellsize
    cat("resolution  : ", cs[1], ", ", cs[2], "  (x, y)\n", 
        sep = "")
  }
  else if (inherits(x, "SpatialGrid")) {
    isRaster <- TRUE
    cr <- x@grid@cells.dim
    cat("dimensions  : ", cr[2], ", ", cr[1], ", ", prod(cr), 
        ", ", nc, "  (nrow, ncol, ncell, nlayers)\n", sep = "")
    cs <- x@grid@cellsize
    cat("resolution  : ", cs[1], ", ", cs[2], "  (x, y)\n", 
        sep = "")
  }
  else {
    ln <- length(x)
    cat("features    :", ln, "\n")
  }
  e <- bbox(x)
  if (ln > 0) {
    cat("extent      : ", e[1, 1], ", ", e[1, 2], ", ", e[2, 
                                                          1], ", ", e[2, 2], "  (xmin, xmax, ymin, ymax)\n", 
        sep = "")
  }
  cat("coord. ref. :", proj4string(x), "\n")
  if (hasData) {
    x <- x@data
    maxnl <- 15
    if (!isRaster) {
      cat("variables   : ", nc, "\n", sep = "")
    }
    print(as_data_frame(x))
    # if (nc > maxnl) {
    #   x <- x[, 1:maxnl]
    # }
    # ln <- colnames(x)
    # if (nc > maxnl) {
    #   ln <- c(ln[1:maxnl], "...")
    #   x <- x[, 1:maxnl]
    # }
    # wrn <- getOption("warn")
    # on.exit(options(warn = wrn))
    # options(warn = -1)
    # r <- apply(x, 2, range, na.rm = TRUE)
    # minv <- as.vector(r[1, ])
    # maxv <- as.vector(r[2, ])
    # if (nc > maxnl) {
    #   minv <- c(minv, "...")
    #   maxv <- c(maxv, "...")
    # }
    # w <- pmax(.nchar(ln), .nchar(minv), .nchar(maxv))
    # w[is.na(w)] <- 2
    # m <- rbind(ln, minv, maxv)
    # for (i in 1:ncol(m)) {
    #   m[, i] <- format(m[, i], width = w[i], justify = "right")
    # }
    # cat("names       :", paste(m[1, ], collapse = ", "), 
    #     "\n")
    # cat("min values  :", paste(m[2, ], collapse = ", "), 
    #     "\n")
    # cat("max values  :", paste(m[3, ], collapse = ", "), 
    #     "\n")
  }
}

#' Sp methods
#' @param object Spatial object
#' @param x Spatial object
#' @param ... ignored
#' @title sp methods
#' @rdname sp-methods
#' @export
setMethod("show", "SpatialPolygonsDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
          
          )

#' @rdname sp-methods
#' @export
setMethod("show", "SpatialLinesDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
)

#' @rdname sp-methods
#' @export
setMethod("show", "SpatialPointsDataFrame", 
          function(object) {
            .print_Spatial(object)
          }
          )

#' @rdname sp-methods
#' @export
setMethod ('print' , 'Spatial', 
           function(x, ...) {
             .print_Spatial(x)
           }
)

