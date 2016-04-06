context("nesting")

## TODO: Rename context
## TODO: Add more tests
library(tidyr)
library(spbabel)
library(maptools)
data(wrld_simpl)
xa <- nest(wrld_simpl)
## ggplot2
library(ggplot2)
library(tidyr)


test_that("nesting works", {
  expect_silent( plot(xa, col = grey(seq(0, 1, length = nrow(xa)))))
  expect_silent(ggplot(xa) + stat_nested())
})
