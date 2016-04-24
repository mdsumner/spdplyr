context("db_df")
nam <- c("Australia", "New Zealand")
library(dplyr)
Objects <- function(x) x$Table$Object
Branches <- function(x) x$Table$Branch
Coords <- function(x) x$Table$Vertex



library(spbabel)
library(maptools)
data(wrld_simpl)
world <- db_df(wrld_simpl)


context("db_df works")
test_that("round trip embedding works", {
  expect_that(Objects(world) %>% select(NAME, object_) %>% filter(NAME %in% nam), is_a("tbl_df"))
})

countryObjects <- Objects(world) %>% select(NAME, object_) %>% filter(NAME %in% nam)
countries <- countryObjects %>% inner_join(Branches(world), "object_") %>% inner_join(Coords(world), "branch_")
test_that("round trip embedding works", {
  expect_that(spFromTable(countries, attr_tab = countryObjects, quiet = TRUE), is_a("SpatialPolygonsDataFrame"))
})
