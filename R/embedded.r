#' Nested tables without recursion :)
#'
#' @return 
#' For Spatial, a data frame with columns
#' \itemize{
#'  \item \code{object} object ID \code{integer}
#'  \item \code{branch}  global branch ID \code{integer}
#'  \item \code{hole} hole status island or hole? \code{logical}
#' }
#' @param x Spatial object
#' @param ... ignored
#'
#' @return nested tables
#' @export
db_df <- function(x, ...) {
  UseMethod("db_df")
}

#' @rdname db_df
#' @export
db_df.list <- function(x, ...) {
  if (is.null(names(x))) names(x) <- make.names(seq_along(x))
  data_frame(Name = names(x), Table = x)
}

#' @rdname db_df
#' @export
db_df.Spatial <- function(x, ...) {
  sptab <-  sptable(x) 
  attrd <- as_data_frame(as.data.frame(x))
  x <- list(Coord = select_(sptab, "x", "y", "branch"), 
       Branch = select_(distinct_(sptab, "branch"), "branch", "hole", "object"), 
       Object = bind_cols(attrd, select_(distinct_(sptab, "object"), "object")))
  db_df(x)
}

