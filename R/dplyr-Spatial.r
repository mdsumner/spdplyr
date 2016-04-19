#' Dplyr verbs for Spatial
#' 
#' @inheritParams dplyr::mutate
#'
#' @rdname spdplyr
#' @export
#' @examples 
#' library(sp)
#' library(maptools)
#' data(wrld_simpl)
#' library(dplyr)
#' library(spbabel)   ## devtools::install_github("mdsumner/spbabel", ref = "pipe")
#' library(raster)  
#' wrld_simpl %>% mutate(NAME = "allthesame", REGION = row_number())
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

#' @rdname spdplyr
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


#' @rdname spdplyr
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


#' @rdname spdplyr
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


#' @rdname spdplyr
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

#' @rdname spdplyr
#' @export
select_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data to select for a %s", class(.data))
  }
 dat <-  select_(as_data_frame(as.data.frame(.data)), ...)
 .data[, names(dat)]
}
  
#' @rdname spdplyr
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



#' @rdname spdplyr
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


#' @rdname spdplyr
#' @export
#' @importFrom rgeos gUnaryUnion
summarise_.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
    stop("no data for distinct for a %s", class(.data))
  }
  # this should only ever return one-row objects
  # we cannot group_by on Spatial
  dat <- summarise_(as_data_frame(as.data.frame(.data)), ...)
  g <- rgeos::gUnaryUnion(geometry(.data))
  #print(class(g))
  x <- .data[1,]
  row.names(x) <- "1"
  x@data <- as.data.frame(dat)
  if (inherits(.data, "SpatialPolygonsDataFrame")) {
    x@polygons <- g@polygons
  }
  if (inherits(.data, "SpatialLinesDataFrame")) {
    x@lines <- g@lines
  }
  ## and so on for points and multipoints
  
  
  
 ## geometry(x) <- g
  x
}


# decided this is a non-starter
#  - a good reason to stop and do this with another scheme
# #' @rdname spdplyr
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

 