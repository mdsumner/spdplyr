#' A basis for converting between different types of spatial objects.
#'
#'
#' The spbabel package provides methods for the dplyr verbs and functions to round-trip a Spatial object to a single table and back.
#'
#' @section I. dplyr verbs:
#'  \tabular{ll}{
#'   \code{\link[dplyr]{filter}} \tab  filter   \cr
#'   \code{\link[dplyr]{slice}} \tab   slice   \cr
#'   \code{\link[dplyr]{arrange}} \tab arrange   \cr
#'   \code{\link[dplyr]{select}} \tab  select   \cr
#'   \code{\link[dplyr]{rename}} \tab  rename   \cr
#'   \code{\link[dplyr]{distinct}} \tab distinct  \cr
#'   \code{\link[dplyr]{mutate}} \tab   mutate  \cr
#'   \code{\link[dplyr]{transmute}} \tab transmute \cr
#'  }
#' @section II. Conversion:
#' \tabular{ll}{
#'  \code{\link{sptable}} \tab create a \code{\link[dplyr]{tbl_df}} from Spatial*DataFrame  \cr
#'  \code{\link{spFromTable}} \tab create Spatial object from table \cr
#'  }
#'
#' @name spbabel-package
#' @docType package
NULL

#' MultiPointsDataFrame data set
#' 
#' Created with the following code
#' #library(maptools)
#' #data(wrld_simpl)
#' #poly1 <- wrld_simpl
#' #line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
#' #point1 <- as(line1, "SpatialPointsDataFrame")
#' #mpoint1 <- SpatialMultiPointsDataFrame(lapply(split(line1, seq(nrow(line1))), function(y) coordinates(as(y, "SpatialPoints"))), 
#'                              ##         as.data.frame(line1))
#' @name mpoint1
#' @docType data
#' @rdname mpoint1
NULL
