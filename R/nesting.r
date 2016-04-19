
#' Two level nesting, branches and objects
#'
#' @param data Spatial*DataFrame
#' @param ... unused
#' @name nest
#' @return nested data frame
#' @export
#' @importFrom tidyr nest_ unnest
#' @examples
#' library(maptools)
#' data(wrld_simpl)
#' library(tidyr)
#' x <- nest(wrld_simpl)
#' x %>% select(Object, ISO3) %>% unnest %>% unnest
#' plot_nsp_df(x, grey(seq(0, 1, length = nrow(x))))
nest_.Spatial <- function(data, ...) {
  sptab <-  sptable(data) %>% 
    group_by_("branch", "object") %>% 
    nest_(key_col = "Branch") %>%  
    group_by_("object") %>% nest_(key_col = "Object")
  
  attrd <- as_data_frame(as.data.frame(data))
  y <- bind_cols(attrd, sptab)
  attr(y, "crs") <- proj4string(data)
  class(y) <- c("nsp_df", class(y))
  y
}

nsp_df <- function(x, ...) UseMethod(x)

from_nested_df <- function(x) {
  spFromTable(vertices(select_(x, "object", "Object")), crs = attr(x, "crs"), attr = x[!sapply(x, is.list)], quiet = TRUE)
}


vertices <- function(x) {
  unnest(unnest(select_(x, "object", "Object")))
}


#' Methods for nsp_df
#' 
#' @param x nsp_df object
#' @param i see \code{\link{Extract}}
#' @param j see \code{\link{Extract}}
#' @param ... ignored
#' @export
#' @rdname nsp_df-methods
`[.nsp_df` <- function(x, i, j, ...) {
  d <- NextMethod("[", x)
  if (!is.null(d[["Object"]]) & is.list(d[["Object"]])) {
    class(d) <- c("nsp_df", class(d))
    
  }
  d
}

#' Methods for as.tbl
#' @inheritParams dplyr::as.tbl
#' @importFrom dplyr as.tbl group_by_
#' @export
#' @rdname as.tbl-methods
as.tbl.nsp <- function(x, ...) {
  class(x) <- setdiff("nsp_df", class(x))
  x
}

#' Filter
#' 
#' @inheritParams dplyr::filter
#'
#' @export
#' @rdname filter-nsp
#' @examples
#' \dontrun{
#' xa <- nestify(wrld_simpl)
#' class(xa %>% filter(NAME %in% c("Australia", "New Zealand")))
#'  class(xa[xa$NAME %in% c("Australia", "New Zealand"), ])
#' }
filter_.nsp_df <- function (.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  x <- as.tbl(filter_(tbl_df(.data), .dots = dots))
  class(x) <- c("nsp_df", class(x))
  x
}


#' @export
#' @importFrom sp plot
plot_nsp_df <- function(x, ...) {
   x <- from_nested_df(x)
   plot(x, ...)
   invisible(NULL)
 }

