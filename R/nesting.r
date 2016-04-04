
#' Two level nesting, branches and objects
#'
#' @param x Spatial*DataFrame
#'
#' @return nested data frame
#' @export
#' @importFrom tidyr nest unnest
#' @examples
#' library(maptools)
#' data(wrld_simpl)
#' x <- as_nested_df(wrld_simpl)
#' x %>% select(Object, ISO3) %>% unnest %>% unnest
#' plot(x, grey(seq(0, 1, length = nrow(x))))
as_nested_df <- function(x) {
  y <- bind_cols(as.data.frame(x), 
            spbabel::sptable(x) %>% 
              group_by(cump, object) %>% 
              nest(.key = "Branch") %>%  
              group_by(object) %>% nest(.key = "Object"))
  attr(y, "crs") <- proj4string(x)
  class(y) <- c("nsp_df", class(y))
  y
}

from_nested_df <- function(x) {
  spbabel::spFromTable(vertices(select_(x, "object", "Object")), crs = attr(x, "crs"), attr = x[!sapply(x, is.list)])
}


vertices <- function(x) {
  unnest(unnest(select_(x, "object", "Object")))
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

