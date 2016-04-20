#' A basis for converting between different types of spatial objects.
#'
#'# @importFrom utils 
#' @aliases NULL
#' @details The spbabel package provides methods for the dplyr verbs and functions to round-trip a Spatial object to a single table and back.
#' 
#' @section O. dplyr verbs:
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
#'  
#' @section I. dplyr-Spatial: dplyr verbs operating directly on Spatial objects
#' @section II. sptable: an extension to fortify in ggplot2 
#' @section III. sp_df: store the Spatial-geometry as a list-column
#' @section IV.  sportify: single-nested fortify tables
#' @section V. nsp_df: double-nested tables for Branch and Vertex
#' The \code{nsp_df} class provides a "nested spatial" way to store the parts and child vertices of Spatial objects in one table. 
#' @section VI. db_df: table normalization#' 
#' 
#' The \code{db_df} class provides a "database table" way to store tables in a single object, the "tables" table. 
#' 
#' @name spbabel-package
#' @docType package
NULL

# @section II. Conversion:
# \tabular{ll}{
#  \code{\link{sptable}} \tab create a \code{\link[dplyr]{tbl_df}} from Spatial*DataFrame  \cr
#  \code{\link{spFromTable}} \tab create Spatial object from table \cr
#  \code{\link{sp_df}} \tab store the Spatial part on the table \cr
#  \code{\link{nest}} \tab create a nested spatial table \cr
#  \code{\link{db_df}} \tab create a table of tables \cr
#  }
#

#' MultiPointsDataFrame data set
#' 

#' @name mpoint1
#' @docType data
#' @rdname mpoint1
NULL

#' "South-east" map data. 
#' 
#' Created in /data-raw/ \code{semap} is the setable() version of some of maptools wrld_simpl, and \code{seatt} is the matching attribute data, linked by `object`. 
#' @name semap
#' @docType data
#' @examples 
#' # recreate as sp object
#' mp <- spFromTable(semap, attr = seatt, crs = "+proj=longlat +ellps=WGS84")
#' 
NULL


#' "South-east" map attribute data. 
#' 
#' Created in /data-raw/
#' @name seatt
#' @docType data
#' @rdname semap
NULL

