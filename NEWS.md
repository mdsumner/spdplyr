# spdplyr 0.4.0

* Add `transmute.Spatial()`. 

* Remove all old dplyr `verbs_()` now deprecated. 


# spdplyr 0.3.0

* Fix for dpylr 0.8.1, can no longer support `mutate_at` and `mutate_if`. 

# spdplyr 0.2.0

* Fix for dplyr 0.8.0. 

# spdplyr 0.1.3

* added methods for dplyr::groups and dplyr::tbl_vars as per https://github.com/mdsumner/spdplyr/issues/10, these
  are dependent on utils::packageVersion("dplyr") > "0.5.0"

* fixed a bug from using '.data@data' within a tibble() call where '.data' has a special meaning

* updated tests based on minor changes in dplyr around expected warning contents

* fixed problem caused by drop = TRUE https://github.com/mdsumner/spdplyr/pull/13

* incorporated PR  https://github.com/mdsumner/spdplyr/pull/12 to get data frame from data slot instead 
  of using as.data.frame, and extra tests
  

# spdplyr 0.1.2

* updated to use sp rather than spFromTable in line with spbabel

* modified the print/show details for Spatial 

* removed some leftover references from original source

# spdplyr 0.1.1

* first working version 

* forked from spbabel 0b95adb3, previous news is from spbabel originally

* various improvements provided by jlegewie, removed transmute_ (not needed), improved filter_ and select_, added left_join and inner_join, see https://github.com/mdsumner/spbabel/pull/10

* added group_by and complementary summarize capability for Spatial 

* set data.frame and tbl and tbl_df as S4 compatible

