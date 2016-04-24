#' Nested tables for Spatial
#'
#' @param data Spatial*DataFrame
#' @param ... unused
#' @return nested data frame
#' @export
#' @importFrom tidyr nest_ unnest
#' @importFrom dplyr group_by_
#' @examples
#' library(maptools)
#' data(wrld_simpl)
#' library(tidyr)
#' x <- nest(wrld_simpl)
#' 
#' x %>% select(Object_, ISO3) %>% unnest()
nest_.Spatial <- function(data, ...) {
  #sptab <-  sptable(data) %>% nest(-object_)
  sptab <- sptable(data) %>% group_by_("object_") %>% nest_(key_col = "Object_")
  attrd <- as_data_frame(as.data.frame(data))
  y <- bind_cols(attrd, sptab)
 # attr(y, "crs") <- proj4string(data)
 #  class(y) <- c("nsp_df", class(y))
  y
}




# #' Nested tables for Spatial
 # #'
 # #' @param data Spatial*DataFrame
 # #' @param ... unused
 # #' @name nsp_df
 # #' @return nested data frame
 # #' @export
 # #' @importFrom tidyr nest_ unnest
 # #' @importFrom dplyr group_by_
 # #' @examples
 # #' library(maptools)
 # #' data(wrld_simpl)
 # #' library(tidyr)
 # #' x <- nest(wrld_simpl)
 # #' x %>% select(Object_, ISO3) %>% unnest %>% unnest
 # nest_.Spatial <- function(data, ...) {
 #   sptab <-  sptable(data) %>%
 #     group_by_("branch_", "object_") %>%
 #     nest_(key_col = "Branch_") %>%
 #     group_by_("object_") %>% nest_(key_col = "Object_")
 # 
 #   attrd <- as_data_frame(as.data.frame(data))
 #   y <- bind_cols(attrd, sptab)
 #   attr(y, "crs") <- proj4string(data)
 #   class(y) <- c("nsp_df", class(y))
 #   y
 # }
 # 

# #' Title
# #'
# #' @param x 
# #' @param ... 
# #'
# #' @return
# #' @export
# #' @importFrom tidyr nest_
# #' @importFrom dplyr bind_cols as_data_frame
# #' @examples
# n1sp_df <- function(x, ...) {UseMethod("n1sp_df")}
# #' @export
# n1sp_df.Spatial <- function(x, ...) {
#   xx <- sptable(x) %>% group_by_("object_") %>% nest_(key_col = "Object_") 
#   bind_cols(as_data_frame(as.data.frame(x)), xx)
#   
# }
# #' Title
# #'
# #' @param x 
# #' @param ... 
# #'
# #' @return
# #' @export
# #'
# #' @examples
# n2sp_df <- function(x, ...) UseMethod("n2sp_df")
# #' @export
# n2sp_df.Spatial <- function(x, ...) {
#   xx <- sptable(x) %>% group_by_("branch_", "object_") %>% nest_(key_col = "Branch_") %>% 
#     group_by_("object_") %>% nest_(key_col = "Object_")
#   
#   bind_cols(as_data_frame(as.data.frame(x)), xx)
# 
#   
# }

