#' A basis for converting between different types of spatial objects.
#'
#'# @importFrom utils 
#' @aliases NULL
#' @details The spbabel package provides methods for the dplyr verbs and functions to round-trip a Spatial object to a single table and back.
#' The \code{db_df} class provides a "database table" way to store tables in a single object, the "tables" table. 
#' 
#' The \code{nsp_df} class provides a "nested spatial" way to store the parts and child vertices of Spatial objects in one table, either all as tables or with the list of Spatial objects in a list. 
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
#'  \code{\link{sp_df}} \tab store the Spatial part on the table
#'  \code{\link{nest}} \tab create a nested spatial table
#'  \code{\link{db_df}} \tab create a table of tables
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

#' "South-east" map data. 
#' 
#' Created in /data-raw/ \code{semap} is the setable() version of some of maptools wrld_simpl, and \code{seatt} is the matching attribute data, linked by `object`. 
#' @name semap
#' @docType data
NULL


#' "South-east" map attribute data. 
#' 
#' Created in /data-raw/
#' @name seatt
#' @docType data
#' @rdname semap
NULL

