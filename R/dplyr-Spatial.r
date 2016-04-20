#' Dplyr verbs for Spatial
#' 
#' Direct application of the dplyr verbs to Spatial objects. Not all verbs are supported, see Details. 
#' 
#' mutate, transmute, filter, arrange, slice, select, rename, distinct all work with attributes on the "data" slot and leave the geometry unchanged. 
#' 
#' summarise collapses to a single geometry by listing all subgeometries together, it does not perform any topological union or merge. 
#' This is a pretty brutal collapse of all the data. 
#' 
#'  summarise for points and multipoints, ... todo single Multipoint for multiple points
#' @inheritParams dplyr::mutate
#' @note Beware that attributes stored on Spatial objects *are not* linked to the geometry. Attributes are often used to store the area or perimeter length or centroid values but these may be completely unmatched to the underlying geometries. 
#' @rdname dplyr-Spatial
#' @name dplyr-Spatial
#' @export
#' @examples 
#' library(sp)
#' library(maptools)
#' data(wrld_simpl)
#' library(dplyr)
#' library(spbabel)   ## devtools::install_github("mdsumner/spbabel", ref = "pipe")
#' library(raster)  
#' wrld_simpl %>% mutate(NAME = "allthesame", REGION = row_number())
#' wrld_simpl %>% transmute(alpha = paste0(FIPS, NAME))
#' wrld_simpl %>% filter(NAME %in% c("New Zealand", "Australia", "Fiji"))
#' wrld_simpl %>% arrange(LON)
#' wrld_simpl %>% slice(c(9, 100))
#' wrld_simpl %>% dplyr::select(UN, FIPS)
#' wrld_simpl %>% rename(`TM_WORLD_BORDERS_SIMPL0.2NAME` = NAME)
#' wrld_simpl %>% distinct(REGION) %>% arrange(REGION)  ## first alphabetically in REGION
#' wrld_simpl %>% arrange(REGION, desc(NAME)) %>% distinct(REGION) ## last
#' 
#' ## we don't need to use piping
#' slice(filter(mutate(wrld_simpl, likepiping = FALSE), abs(LON - 5) < 35 & LAT > 50), 4)
#' 
#' 
#' ## works with Lines
#' #as(wrld_simpl, "SpatialLinesDataFrame") %>% mutate(perim = rgeos::gLength(wrld_simpl, byid = TRUE))
#' 
#' 
#' ## summarise/ze is different, we have to return only one geometry
#' wrld_simpl %>% summarize(max(AREA))
#' @importFrom dplyr mutate_ transmute_ filter_ arrange_ slice_ select_ rename_ distinct_ summarise_
#' @importFrom lazyeval all_dots
mutate_.Spatial <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)

  if (.hasSlot(.data, "data")) {
    dat <- mutate_(as.data.frame(.data), .dots = dots)
  } else {
    stop("no data to mutate for a %s", class(.data))
  }
  .data@data <- dat
  .data
}


#' @rdname dplyr-Spatial
#' @export
#' @importFrom rgeos gUnionCascaded gLineMerge
summarise_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data for distinct for a %s", class(.data))
  }
  # this should only ever return one-row objects
  # we cannot group_by on Spatial
  dat <- summarise_(as_data_frame(as.data.frame(.data)), ...)
 ## print(dat)
  row.names(dat) <- "1"
  gbomb <- sptable(.data)
  gbomb$object <- 1
  ## not
  gbomb$part <- gbomb$branch
  ## only return the same names as the summarized data_frame
  #
  
  spFromTable(gbomb, attr = dat, crs = proj4string(.data))
  
}


#' @rdname dplyr-Spatial
#' @export
transmute_.Spatial <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  if (.hasSlot(.data, "data")) {
    dat <- transmute_(as.data.frame(.data), .dots = dots)
  } else {
    stop("no data to mutate for a %s", class(.data))
  }
 .data@data <- dat
  .data
}


#' @rdname dplyr-Spatial
#' @export
filter_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to filter for a %s", class(.data))
  }
  .data$rnames <- as.character(seq(nrow(.data)))
  #print(rnames)
  if (inherits(.data, "SpatialMultiPointsDataFrame")) {
    dat <- filter_(as_data_frame(.data@data), ...)
  } else {
   dat <- filter_(as_data_frame(as.data.frame(.data)), ...)
  }
#print(row.names(dat))
  asub <- .data$rnames %in% dat$rnames
  .data[asub, ]
}


#' @rdname dplyr-Spatial
#' @importFrom dplyr arrange_ distinct_ rename_ select_ slice_ filter_ transmute_ mutate_ 
#' @export
arrange_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to arrange for a %s", class(.data))
  }
  dat <- as_data_frame(as.data.frame(.data))
  dat$order <- seq(nrow(dat))
  dat <- arrange_(dat, ...)
  .data[dat$order, ]
}


#' @rdname dplyr-Spatial
#' @export
slice_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to slice for a %s", class(.data))
  }
  dat <-  as_data_frame(as.data.frame(.data))
  dat$order <- seq(nrow(dat))
  dat <- slice_(dat, ...)
  .data[dat$order, ]
}

#' @rdname dplyr-Spatial
#' @export
select_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to select for a %s", class(.data))
  }
 dat <-  select_(as_data_frame(as.data.frame(.data)), ...)
 .data[, names(dat)]
}
  
#' @rdname dplyr-Spatial
#' @export
rename_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to rename for a %s", class(.data))
  }
  onames <- names(.data)
  dat <-  rename_(as_data_frame(as.data.frame(.data)), ...)
  names(.data) <- names(dat)
  .data
}



#' @rdname dplyr-Spatial
#' @export
distinct_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data for distinct for a %s", class(.data))
  }
  orownames <- rownames(.data)
  .data$order <- seq(nrow(.data))
  dat <- distinct_(as_data_frame(as.data.frame(.data)), ...)
  .data[dat$order, ]
}

# getsubSpatial <- function(x) {
#   if (inherits(x, "SpatialPolygons")) return(x@polygons)
#   if (inherits(x, "SpatialLInes")) return(x@lines)
#   NULL
# }
# c_ <- function(x) {
#   ##need to fix the IDs
#   do.call("c", getsubSpatial(x))
# }




# decided this is a non-starter
#  - a good reason to stop and do this with another scheme
# #' @rdname dplyr-Spatial
# #' @export
# group_by_.Spatial <- function(.data, ...) {
#   if (!.hasSlot(.data, "data")) {
#     stop("no data for distinct for a %s", class(.data))
#   }
#   orownames <- row.names(.data)
#   dat <- group_by_(as_data_frame(as.data.frame(.data)), ...)
#   groupatts <- attributes(dat)
#   groupatts$class <- "data.frame"
#   groupatts$row.names <- orownames
#   dat <- as.data.frame(dat)
#  
#   attributes(dat) <- groupatts
#   .data@data <- dat
#   .data
# }
# 
# 

 