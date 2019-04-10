context("dplyr-0.6.0")
data("wrld_simpl", package = "maptools")
test_that("tibble requirements", {
#  wrld_simpl %>% mutate_all(list(as.character(.)))
  wrld_simpl %>% mutate_at(vars(starts_with("L")), list(~as.integer(.)))
})
