setOldClass( c("tbl_df", "tbl", "data.frame" ) )
setOldClass( c("grouped_df", "tbl_df", "tbl", "data.frame" ) )

#' dplyr S3 methods
#' 
#' Worker functions used by dplyr features. 
#' 
#' These will work for the Spatial-DataFrame  objects, though not properly for any-Spatial. 
#' @param x input Spatial object
#' @name dplyr-S3
#' @export
#' @importFrom dplyr groups tbl_vars
#' @examples 
#' if (utils::packageVersion("dplyr") > "0.5.0") {
#' 
#' spmap %>% mutate_if(is.numeric, as.character)
#' spmap %>% mutate_all(funs(as.character))
#' spmap %>% mutate_at(vars(starts_with("L")), funs(as.integer))
#' }
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
#' @param .dots Used to work around non-standard evaluation. 
#' @note Beware that attributes stored on Spatial objects *are not* linked to the geometry. Attributes are often used to store the area or perimeter length or centroid values but these may be completely unmatched to the underlying geometries. 
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
#' @importFrom dplyr %>% arrange mutate_ transmute_ filter_ arrange_ slice_ select_ rename_ distinct_ summarise_
#' @importFrom dplyr     mutate
#' @importFrom lazyeval all_dots
NULL
#' @noRd
#' @importFrom tibble as_tibble
data_or_stop <- function(x, mess = "") {
  if (!.hasSlot(x, "data")) {
    stop("no data %s for a %s", mess, class(x))
  } 
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
mutate_.Spatial <-  function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  dat <- data_or_stop(.data, " to mutate ")
  .data@data <- mutate_(dat, .dots = dots)
  .data
}


#' @rdname dplyr-Spatial
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
summarise_.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to summarize ")
  dat <- summarise_(.data@data, ...)
  
  # row.names(dat) <- "1"
  gbomb <- spbabel::sptable(.data)
  if (inherits(.data@data, "grouped_df")) {
    groups <- attr(.data@data, "indices")  ## only robust for single-level group_by for now
    grp_sizes <- attr(.data@data, "group_sizes")
    regroup <- tibble(labs =  unlist(lapply(seq_along(grp_sizes), function(x) rep(x, grp_sizes[x]))), 
                          inds = unlist(groups) + 1)
    
    gbomb <- gbomb  %>% 
      inner_join(regroup, c("object_" = "inds"))  %>% 
      mutate_(object_ = quote(labs))  %>% 
      select_(quote(-labs))  %>% 
      arrange_("object_", "branch_", "order_")
  
    } else {
    gbomb$object_ <- 1
  }
  spbabel::sp(gbomb, attr_tab = dat, crs = proj4string(.data))
  
}

#' @importFrom dplyr summarise
#' @rdname dplyr-Spatial
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr inner_join mutate select
summarise.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to summarize ")

  dat <- summarise(.data@data, ...)
  
  # row.names(dat) <- "1"
  gbomb <- spbabel::sptable(.data)
  
  ## prepare the groups
  if (inherits(.data@data, "grouped_df")) {
    groups <- attr(.data@data, "indices")  ## only robust for single-level group_by for now
    grp_sizes <- attr(.data@data, "group_sizes")
    regroup <- tibble(labs =  unlist(lapply(seq_along(grp_sizes), function(x) rep(x, grp_sizes[x]))), 
                      inds = unlist(groups) + 1)
    
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
#' @importFrom dplyr group_by_
#' @importFrom tibble as_tibble 
#' @rdname dplyr-Spatial
#' @export
group_by_.Spatial <- function(.data, ...) {
  .data@data <- group_by_(data_or_stop(.data, " to group_by "), ...)
  .data
}

#' @importFrom dplyr group_by
#' @importFrom tibble as_tibble 
#' @rdname dplyr-Spatial
#' @importFrom rlang .data
#' @export
group_by.Spatial <- function(.data, ...) {
  .data@data <- group_by(data_or_stop(.data, " to group_by "), ...)
  .data
}


#' @rdname dplyr-Spatial
#' @export
filter_.Spatial <- function(.data, ..., .dots) {
  dat <- data_or_stop(.data, " to filter ")
  
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  masks <- lazyeval::lazy_eval(dots, data = dat)
  subset(.data, Reduce(`&`, masks))
}
#' @importFrom dplyr filter
#' @rdname dplyr-Spatial
#' @export
filter.Spatial <- function(.data, ...) {
   dat <- data_or_stop(.data, " to filter ")
   nam <- new_name_from_these(names(dat))
   dat[[nam]] <- seq_len(nrow(dat))
   dat <- filter(dat, ...)
   .data[dat[[nam]], ]
}


#' @importFrom dplyr arrange_ 
#' @rdname dplyr-Spatial
#' @export
arrange_.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq_len(nrow(dat))
  dat <- arrange_(dat, ...)
  .data <- .data[dat[[nam]], ]
  .data[[nam]] <- NULL
  .data
}

#' @importFrom dplyr arrange 
#' @rdname dplyr-Spatial
#' @export
arrange.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq_len(nrow(dat))
  dat <- arrange(dat, ...)
  .data <- .data[dat[[nam]], ]
  .data[[nam]] <- NULL
  .data
}


#' @rdname dplyr-Spatial
#' @importFrom tibble as_tibble 
#' @export
slice_.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq_len(nrow(dat))
  dat <- slice_(dat, ...)
  .data <- .data[dat[[nam]], ]
  .data[[nam]] <- NULL
  .data
}
#' @rdname dplyr-Spatial
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
#' @rdname dplyr-Spatial
#' @export
select_.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  dat <-  select_(.data@data, ...)
  .data[, names(dat), drop = FALSE]
}
#' @rdname dplyr-Spatial
#' @export
select.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  dat <-  select(.data@data, ...)
  .data[, names(dat), drop = FALSE]
}

#' @rdname dplyr-Spatial
#' @importFrom tibble as_tibble 
#' @export
rename_.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  onames <- names(.data)
  dat <-  rename_(dat, ...)
  names(.data) <- names(dat)
  .data
}
#' @rdname dplyr-Spatial
#' @importFrom tibble as_tibble 
#' @export
rename.Spatial <- function(.data, ...) {
  dat <- data_or_stop(.data, " to filter ")
  onames <- names(.data)
  dat <-  rename(dat, ...)
  names(.data) <- names(dat)
  .data
}

#' @rdname dplyr-Spatial
#' @export
distinct_.Spatial <- function(.data, ..., .keep_all = FALSE) {
  if (!.keep_all) {
    warning("distinct is not supported for Spatial unless .keep_all = TRUE")
  }
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq(nrow(.data))
  dat <- distinct_(dat, ..., .keep_all = .keep_all)
  out <- .data[dat[[nam]], ] 
  out
}
#' @importFrom dplyr distinct
#' @rdname dplyr-Spatial
#' @export
distinct.Spatial <- function(.data, ..., .keep_all = FALSE) {
  if (!.keep_all) {
    warning("distinct is not supported for Spatial unless .keep_all = TRUE")
  }
  dat <- data_or_stop(.data, " to filter ")
  nam <- new_name_from_these(names(dat))
  dat[[nam]] <- seq(nrow(.data))
  dat <- distinct(dat, ..., .keep_all = .keep_all)
  out <- .data[dat[[nam]], ] 
  out
}

#' @rdname dplyr-Spatial
#' @param y tbl to join
#' @importFrom dplyr left_join inner_join
#' @inheritParams dplyr::left_join
#' @export
left_join.Spatial <- function (x, y, by = NULL, copy = FALSE, ...) {
  x@data <- left_join(as_tibble(x@data), y, by = by, copy = copy, ...)
  x
}

#' @rdname dplyr-Spatial
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





