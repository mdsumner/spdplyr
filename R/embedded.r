#' Nested tables without recursion :)
#'
#' @return 
#' For Spatial, a data frame with columns
#' \itemize{
#'  \item \code{object} \tab object ID \code{integer}
#'  \item \code{part} \tab part counter within object \code{integer}
#'  \item \code{branch} \tab global part ID \code{integer}
#'  \item \code{hole} \tab hole status island or hole? \code{logical}
#'  \item \code{x} \tab 
#' }
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
#' @export
#'
#' @examples
db_df.Spatial <- function(x, ...) {
  sptab <-  sptable(x) 
  attrd <- as_data_frame(as.data.frame(x))
  x <- list(Coord = select_(sptab, "x", "y", "branch"), 
       Branch = select_(distinct_(sptab, "branch"), "branch", "hole", "object"), 
       Object = bind_cols(attrd, select_(distinct_(sptab, "object"), "object")))
  db_df(x)
}

