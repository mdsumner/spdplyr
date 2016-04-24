 #' Nested tables for Spatial
 #'
 #' @param data Spatial*DataFrame
 #' @param ... unused
 #' @name nsp_df
 #' @return nested data frame
 #' @export
 #' @importFrom tidyr nest_ unnest
 #' @importFrom dplyr group_by_
 #' @examples
 #' library(maptools)
 #' data(wrld_simpl)
 #' library(tidyr)
 #' x <- nest(wrld_simpl)
 #' x %>% select(Object_, ISO3) %>% unnest %>% unnest
 nest_.Spatial <- function(data, ...) {
   sptab <-  sptable(data) %>%
     group_by_("branch_", "object_") %>%
     nest_(key_col = "Branch_") %>%
     group_by_("object_") %>% nest_(key_col = "Object_")

   attrd <- as_data_frame(as.data.frame(data))
   y <- bind_cols(attrd, sptab)
   attr(y, "crs") <- proj4string(data)
   class(y) <- c("nsp_df", class(y))
   y
 }
 
 
