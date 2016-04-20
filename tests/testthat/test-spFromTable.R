context("spFromTable")
library(maptools)
library(dplyr)
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")

sptab <- sptable(poly1)
sp <- spFromTable(sptab, attr = as.data.frame(poly1))

test_that("we can round-trip sensibly", {
  expect_true(all(names(poly1) == names(sp)))
  expect_that(nrow(poly1), equals(nrow(sp)))
  expect_true(proj4string(sp) == gsub("^ ", "", proj4string(poly1)))
})

sptab$new <- runif(nrow(sptab))
sp1 <- spFromTable(sptab, attr = as.data.frame(poly1))
test_that("attributes are preserved, and adding a new one does only that", {
  expect_true(all(names(poly1) == names(sp)))
  expect_that(setdiff( names(sp1), names(poly1)), equals("new"))
})
