setOldClass( c("tbl_df", "tbl", "data.frame" ) )
setOldClass( c("grouped_df", "tbl_df", "tbl", "data.frame" ) )
#' @importFrom utils globalVariables
utils::globalVariables("group_rows")
#' dplyr S3 methods
#'
#' Worker functions used by dplyr features.
#'
#' These will work for the Spatial-DataFrame  objects, though not properly for any-Spatial.
#' @param x input Spatial object
#' @name dplyr-S3
#' @export
#' @importFrom dplyr groups tbl_vars
tbl_vars.Spatial <- function(x) names(x)
#' @name dplyr-S3
#' @export
groups.Spatial <- function(x) NULL

#' Dplyr verbs for Spatial
#'
#' Direct application of the dplyr verbs to Spatial objects. There is no need for a conversion from and to Spatial with this approach. Not all verbs are supported, see Details.
#'
#' mutate, transmute, filter, arrange, slice, select, rename, distinct all work with attributes on the "data" slot and leave the geometry unchanged.
#'
#' summarise collapses to a grouped geometries by listing all subgeometries together, it does not perform any topological union or merge, and it takes no account of the calculations done on attributes.
#' This is a brutal collapse of all the data, and is identical to what is seen with spplot(x, "group"). The behaviour of geometric collapse like this
#' is touch and go anyway, see the examples for a what `rgeos::gUnion` does.
#'
#'  summarise for points and multipoints, ... todo single Multipoint for multiple points
#' @param .data A tbl.
#' @param ... Name-value pairs of expressions. See \code{\link[dplyr]{mutate_}}
#' @param .keep_all argument for \code{\link[dplyr]{distinct}}, we have to set it to TRUE
#' @note Beware that attributes stored on Spatial objects *are not* linked to the geometry. Attributes are often used to
#' store the area or perimeter length or centroid values but these may be completely unmatched to the underlying geometries.
#' @section Warning:
#' `distinct` uses behaviour identical to `duplicated`, by coercing all the relevant values to text and determining uniqueness
#' from those. `dplyr::distinct` uses a different internal method that will give different results for some cases of numeric data.
#' @rdname dplyr-Spatial
#' @name dplyr-Spatial
#' @examples
#' library(sp)
#' library(maptools)
#' data(wrld_simpl)
#' library(spdplyr)
#' library(raster)
#' wrld_simpl %>% mutate(NAME = "allthesame", REGION = row_number())
#' wrld_simpl %>% transmute(alpha = paste0(FIPS, NAME))
#' wrld_simpl %>% filter(NAME %in% c("New Zealand", "Australia", "Fiji"))
#' \dontrun{
#' wrld_simpl %>% arrange(LON)
#' wrld_simpl %>% slice(c(9, 100))
#' wrld_simpl %>% dplyr::select(UN, FIPS)
#' wrld_simpl %>% rename(`TM_WORLD_BORDERS_SIMPL0.2NAME` = NAME)
#' wrld_simpl %>% distinct(REGION, .keep_all = TRUE) %>%
#'    arrange(REGION)  ## first alphabetically in REGION
#' wrld_simpl %>% arrange(REGION, desc(NAME)) %>% distinct(REGION, .keep_all = TRUE) ## last
#' }
#' ## we don't need to use piping
#' slice(filter(mutate(wrld_simpl, likepiping = FALSE), abs(LON - 5) < 35 & LAT > 50), 4)
#'
#'
#' ## works with Lines
#' #as(wrld_simpl, "SpatialLinesDataFrame") %>%
#'  # mutate(perim = rgeos::gLength(wrld_simpl, byid = TRUE))
#'
#' \dontrun{
#' ## summarise/ze can be used after group_by, or without
#' wrld_simpl %>% filter(REGION == 150) %>% summarize(max(AREA))
#' wrld_simpl %>% group_by(REGION) %>% summarize(max(AREA)) %>%
#' plot(col = rainbow(nlevels(factor(wrld_simpl$REGION)), alpha = 0.3))
#' }
#' @importFrom dplyr %>%    arrange mutate transmute filter slice select rename distinct summarise
NULL
#' @noRd
#' @importFrom tibble as_tibble
data_or_stop <- function(x, mess = "") {
  if (!.hasSlot(x, "data")) {
    stop("no data %s for a %s", mess, class(x))
  }
  if (inherits(x@data, "tbl_df")) return(x@data)
  as_tibble(x@data)
}
#' @export
#' @name dplyr-Spatial
mutate.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to mutate ")
  .data@data <- mutate(dat, ...)
  .data
}
#' @export
#' @name dplyr-Spatial
transmute.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to mutate ")
  .data@data <- transmute(dat, ...)
  .data
}



#' @name dplyr-Spatial
#' @export
#' @importFrom dplyr inner_join
#' @importFrom spbabel sptable sp
#' @importFrom tibble tibble
#' @examples
#' ## group_by and summarize
#'
#' \dontrun{
#' g <- wrld_simpl  %>% group_by(REGION)  %>%
#'  summarize(alon = mean(LON), mxlat = max(LAT), mxarea = max(AREA))
#' g %>% mutate(ar = factor(REGION)) %>% spplot("ar")
#' w <- wrld_simpl
#' w$ar <- factor(w$REGION)
#' spplot(w, "ar")
#' }
#' \dontrun{
#' # compare what rgeos gives
#' ##spplot(rgeos::gUnionCascaded(w, id = w$ar))  ## good grief, is this compelling . . .
#' ## this is hardly a clean dissolve
#' ##plot(rgeos::gUnionCascaded(w, id = w$ar), col = rainbow(nlevels(factor(w$ar)), alpha = 0.5))
#' }
#' @importFrom dplyr summarise
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join mutate select
#' @importFrom utils packageVersion
#' @rawNamespace
#' if(utils::packageVersion("dplyr") >= "0.8.0") {
#' importFrom(dplyr, group_rows)
#' }
summarise.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to summarize ")

  dat <- summarise(dat, ...)

  # row.names(dat) <- "1"
  gbomb <- spbabel::sptable(.data)

  ## prepare the groups
  if (inherits(.data@data, "grouped_df")) {
    groups <- attr(.data@data, "indices")  ## only robust for single-level group_by for now
    if (is.null(groups)) {
     if (utils::packageVersion("dplyr") >= "0.8.0")  {
       groups <- group_rows(.data@data)
     } else {
       groups <- attr(.data@data, "groups")[[".rows"]]
     }
    }
    grp_sizes <- dplyr::group_size(.data@data)
    if (is.null(grp_sizes)) {
      if (utils::packageVersion("dplyr") >= "0.8.0")  {
        grp_sizes <- group_rows(.data@data)
      } else {
        groups <- lengths(attr(.data@data, "groups")[[".rows"]])
      }
    }
    regroup <- tibble(labs =  unlist(lapply(seq_along(grp_sizes), function(x) rep(x, grp_sizes[x]))),
                      inds = unlist(groups))

    gbomb <- gbomb  %>%
      dplyr::inner_join(regroup, c("object_" = "inds"))  %>%
      dplyr::mutate(object_ = .data$labs)  %>%
     dplyr::select(-.data$labs)  %>%
      dplyr::arrange(.data$object_, .data$branch_, .data$order_)

  } else {
    gbomb[["object_"]] <- 1
  }
  spbabel::sp(gbomb, attr_tab = dat, crs = proj4string(.data))

}


#' @importFrom dplyr group_by
#' @importFrom tibble as_tibble
#' @name dplyr-Spatial
#' @importFrom rlang .data
#' @export
group_by.Spatial <- function(.data, ...) {
  .data@data <- group_by(data_or_stop(.data, " to group_by "), ...)
  .data
}


#' @importFrom dplyr filter
#' @name dplyr-Spatial
#' @export
filter.Spatial <- function(.data, ...) {
   dat <- data_or_stop(.data, " to filter ")
   nam <- new_name_from_these(names(dat))
   dat[[nam]] <- seq_len(nrow(dat))
   dat <- filter(dat, ...)
   .data[dat[[nam]], ]
}



#' @importFrom dplyr arrange
#' @name dplyr-Spatial
#' @export
arrange.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to arrange ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq_len(nrow(dat))
  dat <- arrange(dat, ...)
  .data <- .data[dat[[nam]], ]
  .data[[nam]] <- NULL
  .data
}



#' @name dplyr-Spatial
#' @importFrom dplyr slice slice_
#' @export
slice.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq_len(nrow(dat))
  dat <- slice(dat, ...)
  .data <- .data[dat[[nam]], ]
  .data[[nam]] <- NULL
  .data
}

#' @importFrom dplyr select
#' @name dplyr-Spatial
#' @export
select.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  dat <-  select(.data@data, ...)
  .data[, names(dat), drop = FALSE]
}


#' @name dplyr-Spatial
#' @importFrom dplyr rename
#' @export
rename.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  onames <- names(.data)
  dat <-  rename(dat, ...)
  names(.data) <- names(dat)
  .data
}


#' @importFrom dplyr distinct
#' @name dplyr-Spatial
#' @export
distinct.Spatial <- function(.data, ..., .keep_all = FALSE) {
  dat <- data_or_stop(.data, " to distinct")
  ## use the inputs to isolate the columns
  dat <- dplyr::select(dat, ...)
  fac <- do.call(paste, dat)
  out <- .data[!duplicated(fac), ]
  if (!.keep_all) out <- out[, names(dat)]
  out
}



#' @name dplyr-Spatial
#' @param y tbl to join
#' @importFrom dplyr left_join inner_join
#' @inheritParams dplyr::left_join
#' @export
left_join.Spatial <- function (x, y, by = NULL, copy = FALSE, ...) {
  x@data <- left_join(as_tibble(x@data), y, by = by, copy = copy, ...)
  x
}

#' @name dplyr-Spatial
#' @export
 inner_join.Spatial <- function (x, y, by = NULL, copy = FALSE, ...) {
   randomkey <- paste(sample(c(letters, 1:100)), collapse = "")
   ## kludge to record which rows are kept
   x[[randomkey]] <- seq(nrow(x))
   .data <- inner_join(as_tibble(x@data), y, by = by, copy = copy, ...)
   x <- x[.data[[randomkey]], ]
   .data[[randomkey]] <- NULL
   x@data <- .data
   x
 }





