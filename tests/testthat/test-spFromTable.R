context("spFromTable")
library(maptools)
library(dplyr)
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")

sptab <- sptable(poly1)
sp <- spFromTable(sptab, attr_tab = as.data.frame(poly1))

sptabmod <- sptab %>% mutate(object_ = branch_)
spFromTable(sptabmod)
test_that("we can round-trip sensibly", {
  expect_true(all(names(poly1) == names(sp)))
  expect_that(nrow(poly1), equals(nrow(sp)))
  
  # gah these are such a pain
  #expect_true(proj4string(sp) == gsub("^ ", "", proj4string(poly1)))
})

test_that("can rebuild without attributes", {
  expect_that(spFromTable(sptab), is_a(class(poly1)))
})

sptab$new <- runif(nrow(sptab))
sp1 <- spFromTable(sptab, attr_tab = as.data.frame(poly1))
test_that("attributes are preserved, and adding a new one does only that", {
  expect_true(all(names(poly1) == names(sp)))
  expect_that(setdiff( names(sp1), names(poly1)), equals("new"))
  
})

test_that("mismatched attributes and object number is an error", {
  expect_error(spFromTable(sptab, attr_tab = data.frame(x = sample(1:20, 1))), "number of rows in attr must match distinct object in x")
})

sptable(poly1) <- sptable(poly1) %>% mutate(x_ = x_ - 5)
test_that("replacement sptable works", {
  expect_that(poly1, is_a("SpatialPolygonsDataFrame"))
})
