
has_data <- function(x) .hasSlot(x, "data")


#' @importFrom sp bbox proj4string 
#' @importFrom dplyr  group_by
#' @importFrom tibble as_tibble
.print_Spatial <- 
function (x, ...) 
{
  cat("class       :", class(x), "\n")
  hasData <- FALSE
  nc <- 0

  if (has_data(x)) {
    hasData <- TRUE
    nc <- ncol(x@data)
  }
  ln <- 1
  ln <- length(x)
  cat("features    :", ln, "\n")

  e <- bbox(x)
  if (ln > 0) {
    cat("extent      : ", e[1, 1], ", ", e[1, 2], ", ", e[2, 
                                                          1], ", ", e[2, 2], "  (xmin, xmax, ymin, ymax)\n", 
        sep = "")
  }
  cat("coord. ref. :", proj4string(x), "\n")
  if (hasData) {
    x <- x@data
    cat("variables   : ", nc, "\n", sep = "")
    if (!inherits(x, "tbl_df")) x <- as_tibble(x)
    print(x)
  } 
  invisible(NULL)
}

#' Sp methods
#' @param object Spatial object
#' @title sp methods
#' @importFrom methods show .hasSlot
#' @rdname sp-methods
#' @export
setMethod ('show' , 'Spatial', 
           function(object) {
             .print_Spatial(object)
           }
)
#' @rdname sp-methods
#' @export
setMethod ('show' , 'SpatialPoints', 
           function(object) {
             .print_Spatial(object)
           }
)


release_questions <- 
  c("Have you single-quoted package and other non-English names in the DESCRIPTION and TITLE?\nhttps://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file", 
  c("Have you manually inspected your rendered vignettes?"))
    
    