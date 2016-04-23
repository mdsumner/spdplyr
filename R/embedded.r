#' Nested tables without recursion :)
#'
#' @return 
#' For Spatial, a data frame with columns
#' \itemize{
#'  \item \code{object_} object ID \code{integer}
#'  \item \code{branch_}  global branch ID \code{integer}
#'  \item \code{island_}  ring status island or hole? \code{logical}
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
  x <- list(Coord = select_(sptab, "x_", "y_", "branch_"), 
       Branch = select_(distinct_(sptab, "branch_"), "branch_", "island_", "object_"), 
       Object = bind_cols(attrd, select_(distinct_(sptab, "object_"), "object_")))
  db_df(x)
}

