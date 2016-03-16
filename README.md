<!-- README.md is generated from README.Rmd. Please edit that file -->
spbabel
-------

Part of a long-dreamed of "babelfish" basis for the Spatial classes and the spatial-zoo in R. Inspired by Eonfusion and dplyr so we can treat Spatial like DB rather than Special.

Not ready for use.

Functions `sptable` and `spFromTable` create `tbl_df`s from Spatial classes and round trip them. This is modelled on `raster::geom rather` than `ggplot2::fortify`, but perhaps only since I use raster constantly and ggplot2 barely ever.

Complicating factors are the rownames of sp and the requirement for them both on object IDs and data.frame rownames, and the sense in normalizing the geometry to the table of coordinates without copying attributes.

(See `mdsumner/gris` for normalizing further to unique vertices and storing parts and objects and vertices on different tables.)
