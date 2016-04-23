library(maptools)
data(wrld_simpl)
polytab <- spbabel::sptable(wrld_simpl)
polynames <- c("object_", "branch_", "island_", "order_", "x_", "y_")
polytypes <- setNames(c("integer", "integer", "logical", "integer", "numeric", "numeric"), polynames)

linetab <- spbabel::sptable(as(wrld_simpl, "SpatialLinesDataFrame"))
linenames <- c("object_", "branch_",  "order_", "x_", "y_")
linetypes <- setNames(c("integer",  "integer",  "integer", "numeric", "numeric"), linenames)

context("safety catch in case the column order changes")
test_that("sptable names is the same", {
  expect_equal(names(polytab), polynames)
  expect_equal(names(linetab), linenames)
  
})



context("sptable")
test_that("sptable structure is sound", {
  expect_equal(sort(names(polytab)), sort(polynames))
  expect_equal(sapply(polytab, class)[polynames], polytypes)
  
  expect_equal(sort(names(linetab)), sort(linenames))
  expect_equal(sapply(linetab, class)[linenames], linetypes)
})


nam <- c("Australia", "New Zealand")
library(dplyr)
Objects <- function(x) x$Table$Object
Branches <- function(x) x$Table$Branch
Coords <- function(x) x$Table$Coord



library(spbabel)
library(maptools)
data(wrld_simpl)
world <- db_df(wrld_simpl)


context("embedding works")
test_that("round trip embedding works", {
  expect_that(Objects(world) %>% select(NAME, object_) %>% filter(NAME %in% nam), is_a("tbl_df"))
})

countryObjects <- Objects(world) %>% select(NAME, object_) %>% filter(NAME %in% nam)
countries <- countryObjects %>% inner_join(Branches(world), "object_") %>% inner_join(Coords(world), "branch_")
test_that("round trip embedding works", {
  expect_that(spFromTable(countries, attr_tab = countryObjects, quiet = TRUE), is_a("SpatialPolygonsDataFrame"))
})
