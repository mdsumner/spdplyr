#' Nested tables without recursion :)
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
db_df <- function(x, ...) {
  UseMethod("db_df")
}


#' Create a data frame from a list. 
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
db_df.list <- function(x, ...) {
  if (is.null(names(x))) names(x) <- make.names(seq_along(x))
  data_frame(Name = names(x), Table = x)
}

#' Title
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
db_df.Spatial <- function(x, ...) {
  sptab <-  sptable(x) 
  attrd <- as_data_frame(as.data.frame(x))
  x <- list(Coord = select_(sptab, "x", "y", "cump"), 
       Branch = select_(distinct_(sptab, "cump"), "cump", "hole", "object"), 
       Object = bind_cols(attrd, select_(distinct_(sptab, "object"), "object")))
  
  db_df(x)
}

