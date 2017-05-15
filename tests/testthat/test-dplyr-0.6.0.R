context("dplyr-0.6.0")
data("wrld_simpl", package = "maptools")
test_that("tibble requirements", {
  wrld_simpl %>% mutate_all(funs(as.character))
  wrld_simpl %>% mutate_at(vars(starts_with("L")), funs(as.integer))
})
