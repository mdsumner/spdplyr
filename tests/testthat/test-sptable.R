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


