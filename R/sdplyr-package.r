#' Data Manipulation Verbs for Spatial Classes.
#'
#' @aliases NULL
#' @details The spdplyr package provides methods for the dplyr verbs. 
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
#'   \code{\link[dplyr]{group_by}} \tab group_by \cr
#'   \code{\link[dplyr]{summarise}} \tab summarise (or summarize) \cr
#'  }
#'  
#' @section I. dplyr-Spatial: dplyr verbs operating directly on Spatial objects
#' The implementation of \code{group_by} requires that sp objects are able use the object extending class "data.frame", 
#' \code{summarise} acts on the attributes in the table according to the expressions use and on the geometry by performing a non-topological union. 
#' @name spdplyr-package
#' @docType package
NULL


#' MultiPointsDataFrame data set
#' 

#' @name mpoint1
#' @docType data
#' @rdname mpoint1
NULL

#' "South-east" map data. 
#' 
#' Created in /data-raw/ \code{spmap} is the setable() version of some of maptools wrld_simpl, and \code{seatt} is the matching attribute data, linked by `object`. 
#' @name spmap
#' @docType data
#' @examples 
#' library(dplyr)
#' spmap %>% filter(NAME == "Antarctica")
NULL

