## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
library(tidyr)
library(maptools)
library(dplyr)
data(wrld_simpl)

## ------------------------------------------------------------------------
library(ggplot2)
x <- bind_cols(as.data.frame(wrld_simpl), as_data_frame(as.data.frame(raster::geom(wrld_simpl))) %>% group_by(cump, object) %>% nest %>%  group_by(object) %>% nest)

## ---- eval = FALSE-------------------------------------------------------
#  vertices <- function(x) {
#    unnest(unnest(x))
#  }
#  ## plot function
#  plotspdf <- function(x, ...) {
#    plot(spbabel::spFromTable(vertices(x)))
#  }
#  
#  plotspdf(x)
#  

