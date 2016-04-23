# library(testthat)
# 
# context("sp_df")
# 
# ## TODO: Rename context
# ## TODO: Add more tests
# # mutate, transmute, filter, arrange, slice, select, rename, distinct
# library(maptools)
# data(wrld_simpl)
# test_that("basic construct works", {
#   sp_df(wrld_simpl) %>% 
#     expect_s3_class("tbl_df") %>% 
#     expect_length(length(names(wrld_simpl)) + 1)
#    
#   sp_df(wrld_simpl) %>% select(NAME, Spatial_) %>% 
#     expect_length(2)
#   
# })
# 
# spdf <- sp_df(wrld_simpl)
# filter(spdf, NAME == "Australia")
# #spdf %>% mutate(Spatial_ = as(Spatial_, "SpatialLines"))
# spdf$Spatial_ <- as(spdf$Spatial_, "SpatialLines")
# test_that("all supported verbs ok", {
#   spdf %>% select(NAME, POP2005, Spatial_) %>% 
#     expect_s3_class("tbl_df") %>% 
#     #mutate(Country = NAME) %>% ## nope
#     #transmute(Spatial_ = Spatial_) ## nope
#     filter(POP2005 < 100) %>% 
#     arrange(POP2005)
#   
#   
#   
# }
#           )