context("various")
library("maptools")
library("spdplyr")

data(wrld_simpl)
DF <- wrld_simpl@data %>% transmute(NAME, a = 1)


test_that("multiplication works", {
  testthat::skip("skipping join tests")
  wrld_simpl %>% left_join(DF, by = "NAME")
  wrld_simpl %>% inner_join(DF, by = "NAME")
  wrld_simpl %>% right_join(DF, by = "NAME")
  wrld_simpl %>% full_join(DF, by = "NAME")
  wrld_simpl %>% semi_join(DF, by = "NAME")
  wrld_simpl %>% anti_join(DF, by = "NAME")
  
  wrld_simpl %>% distinct(REGION)
})
