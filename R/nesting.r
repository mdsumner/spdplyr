
#' Two level nesting, branches and objects
#'
#' @param data Spatial*DataFrame
#'
#' @return nested data frame
#' @export
#' @importFrom tidyr nest unnest
#' @examples
#' library(maptools)
#' data(wrld_simpl)
#' x <- nest(wrld_simpl)
#' x %>% select(Object, ISO3) %>% unnest %>% unnest
#' plot(x, grey(seq(0, 1, length = nrow(x))))
nest_.Spatial <- function(data, ...) {
  sptab <-  sptable(data) %>% 
    group_by(cump, object) %>% 
    nest_(key_col = "Branch") %>%  
    group_by(object) %>% nest_(key_col = "Object")
  
  attrd <- as_data_frame(as.data.frame(data))
  y <- bind_cols(attrd, sptab)
  attr(y, "crs") <- proj4string(data)
  class(y) <- c("nsp_df", class(y))
  y
}


from_nested_df <- function(x) {
  spFromTable(vertices(select_(x, "object", "Object")), crs = attr(x, "crs"), attr = x[!sapply(x, is.list)])
}


vertices <- function(x) {
  unnest(unnest(select_(x, "object", "Object")))
}


#' Title
#'
#' @param x 
#' @param i 
#' @param j 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
`[.nsp_df` <- function(x, i, j, ...) {
  d <- NextMethod("[", x)
  if (!is.null(d[["Object"]]) & is.list(d[["Object"]])) {
    class(d) <- c("nsp_df", class(d))
    
  }
  d
}

as.tbl.nsp <- function(x, ...) {
  class(x) <- setdiff("nsp_df", class(x))
  x
}

#' Title
#'
#' @param .data 
#' @param ... 
#' @param .dots 
#'
#' @return
#' @export
#'
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


#' Plot nested Spatial
#'
#' @param x 
#' @param ... 
#'
#' @return \code{NULL}
#' @export
#'
plot.nsp_df <- function(x, ...) {
  x <- from_nested_df(x)
  plot(x, ...)
  invisible(NULL)
}

