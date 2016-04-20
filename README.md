[![Travis-CI Build Status](https://travis-ci.org/mdsumner/spbabel.svg?branch=master)](https://travis-ci.org/mdsumner/spbabel)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Installation
------------

Spbabel can be installed directly from github:

``` r
devtools::install_github("mdsumner/spbabel")
```

spbabel: bisque or bouillon?
============================

Part of an effort towards dplyr/table-fication of Spatial classes and the spatial-zoo in R. Inspired by Eonfusion and Manifold and dplyr and Douglas Adams and spurned on by helpful twitter and ropensci discussions.

What is Spatial in R?
=====================

Spatial data in the `sp` package have a formal definition (extending class `Spatial`) that is modelled on shapefiles, and close at least in spirit to the Simple Features definition. <https://github.com/edzer/sfr>

In GIS we have Objects linked to a table of attributes, one row of a table for each Object. An Object is a shape defined by potentially complex *geometry*, and importantly the geometry is an *inherent* property of the shape while the attribute data is simply Internally Spatial objects are composed of lists of lists of matrices, with two levels of structure for the geometry for each each line or polygon object. An individual matrix is an ordered set of connected vertices defining a single linear path. If the path is closed it is a polygon "ring", and the order of the coordinates (clockwise or anti-clockwise reflects its status as an "island" or a "hole"). The formal base-level classes `Line` and `Polygon` store this path for the corresponding type, (these types cannot be mixed in a composite object). An object may be composed of one or more linear paths of either type, and these are collected together as `Lines` and `Polygons`. So, a single `Line` or `Polygon` corresponds to a piece (connected line, island or hole), and a single `Lines` or `Polygons` object corresponds to a row in the attribute table. The `SpatialPolygon`, `SpatialLines` classes act like vectors of the `Polygons` and `Lines` type, with additional `Spatial` information, the coordinate system in use and the bounding box.

These classes are extremely powerful and provide a seamless mechanism to do many kinds of spatial modelling and spatial data manipulation. However, the definitions are actually very narrow and the design philosophy is generally inflexibility and very formal which means there's often only one way to do a particular task and it can be quite torturous.

How does spbabel allow sp to work with dplyr?
=============================================

There are several ways to do this.

-   **dplyr-Spatial** Write the dplyr verbs for the Spatial classes (runs into limits with sp not using actual data.frame.
-   **sptable** Use the table of coordinates with identifiers for object and part, like fortify, with sptable() and sptable()&lt;- workflow to fortify and modify in a more flexible way.
-   Use a real data.frame but include the Spatial\* objects as a list column. This runs into issues with the meaning of repeated elements.
-   Nest (with tidyr) the fortify table for each object in a single column.
-   Nest (with tidyr) the twice-fortified table for each object, and then in the object table for each part. This is close to full normalization.
-   Nest (with tidyr) the normalized tables Vertices, Objects and the topological links between them.

Please note that the "pipelining" aspect of `dplyr` is not the main motivation here, that is just a syntactic sugar, all of this work can be done in the standard function "chaining" way that is common in R. It's the generalization, speed, database-back-end-ability and need for flexibility in what Spatial provides that is key here.

This document illustrates each of these approaches, work in progress.

Direct dplyr verbs for Spatial
==============================

Apply `dplyr` verbs to the attribute data of `sp` objects with dplyr verbs.

See `?dplyr-Spatial'` for supported verbs.

``` r
data(quakes)
library(sp)
coordinates(quakes) <- ~long+lat
library(spbabel)
## plot a subset of locations by number of stations
quakes %>% dplyr::filter(mag <5.5 & mag > 4.5) %>% select(stations) %>% spplot
```

![](figure/README-unnamed-chunk-3-1.png)<!-- -->

We can use polygons and lines objects as well.

``` r
library(maptools)
#> Warning: package 'maptools' was built under R version 3.2.5
#> Checking rgeos availability: TRUE
data(wrld_simpl)
## put the centre-of-mass centroid on wrld_simpl as an attribute and filter/select
x <- wrld_simpl %>% mutate(lon = coordinates(wrld_simpl)[,1], lat = coordinates(wrld_simpl)[,2]) %>% filter(lat < -20, lon > 60) %>% select(NAME)
plot(x, asp = ""); text(coordinates(x), label = x$NAME, cex = 0.6)
```

![](figure/README-unnamed-chunk-4-1.png)<!-- -->

``` r

wrld_simpl %>% as("SpatialLinesDataFrame") %>% summarise(big = max(AREA))
#> class       : SpatialLinesDataFrame 
#> features    : 1 
#> extent      : -180, 180, -90, 83.57027  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 
#> variables   : 1
#> Source: local data frame [1 x 1]
#> 
#>       big
#>     (int)
#> 1 1638094
```

use the `sptable<-` replacment method to modify the underlying geometric attributes (here `x` and `y` is assumed no matter what coordinate system).

``` r
## standard dplyr on this S4 class
w2 <- filter(wrld_simpl, NAME == "Australia")
plot(w2, col = "grey")

## modify the geometry on this object
sptable(w2) <- sptable(w2) %>% mutate(x = x - 5)

plot(w2, add = TRUE)
```

![](figure/README-unnamed-chunk-5-1.png)<!-- -->

We can also restructure objects.

``` r
## explode (disaggregate) objects to individual polygons
## here each part becomes and object, and each object only has one part
#w3 <- spFromTable(sptable(w2)  %>% mutate(object = part, part = 1), crs = proj4string(w2))
```

How to dplyr with Spatial?
--------------------------

This has been discussed here:

<https://github.com/ropensci/unconf16/issues/22>

I'm also working on a completely database-ready form of spatial objects here:

<https://github.com/mdsumner/gris>

It's not clear to me yet whether we should have just one, or all, or many of these approaches . . . I was not aware of nested data frames though, so exploring that as well.

Here I refer to the *Spatial Family* as the packages sp, raster, rgdal and rgeos. Together these provide very comprehensive support for read and write, coordinate system transformation, extraction and analysis with spatial data, and topological and geometric operations.

What options are there?
-----------------------

We could serialize to WKT and store this in a column, but that means we still need to write classes for the objects and for the column type, so we can dispatch to the already defined methods in the Spatial Family. We could serialize to WKB, but that means we are storing objects in a list in a data frame which adds another level of complexity. This requires constantly serializing/de-serializing between WKT/WKB and Spatial form. One advantage here is that this is how a database (and GDAL) will understand the geometry, both for read and write.

To leverage the power of the Spatial Family we really need easy conversion to the exising Spatial classes. There are three main options that I can see:

1.  Vertex-table-centric: Store the full vertex table with object, 'piece', hole-status, ring ordering and coordinates in one table. Use the object ID to join to other attributes. This is the `ggplot2::fortify` approach.
2.  Data-table-centric: store the object attributes with a link to the geometric information. This could be done by serializing to a column, but still we need conversions and classing to form high-level operations.
3.  Create a new object with multiple tables, at least two for objects and vertices (ggplot2) and up to 6 for vertices, branches, objects and link-tables (gris).

The problem with ggplot2
------------------------

The main issue with the `ggplot2::fortify model` is that it currently doesn't support multiple holes in one polygon. This is not a problem for multi-part line object or for a multi-point object, but since ggplot2 uses gridPoly rather than gridPath this means we cannot get complex holy polygons. The plotting currently fools you by not showing the border, which you would see tracing correctly around a single-hole. We get topological artefacts if this is tried with more than one hole.

This could be fixed using `geom_holygon` as here: but this is not officially supported yet. My guess is this will stay that way until ggvis takes over. Ggvis is another reason I think that 1) is not the right way forward.

The raster package also has function `geom` for the 1) case above, but this creates a matrix rather than a data frame and uses slightly different terms for the object, 'piece', hole, and coordinates.

The problem with spbabel
------------------------

The spbabel package also does a similar task to fortify with the function `sptable`, with the process being reversable with `spFromTable`. The problem here is that we need to store the coordinate system and topological type with the table, and since we don't record the crs or the type per object that requires more thought. Once in a set of tables, there's no reason not to mix lines, points, and areas in one table - and the same goes for coordinate systems. The graphics device knows its coordinate system so like in a GIS we can throw any kind of object at it, and it computes the transformation on the fly and draws it the right way.

The problem with recursive objects
----------------------------------

Sp stores all the geometry in recursive lists of matrices. This means we are stuck to the data type of all vertices (i.e. we cannot have integer, or date-time vertices) and we cannot have 3 or more coordinate attributes without writing new classes of objects. We also have to write specific object-aware methods that recurse the objects in the right way. This problem also exists for the fortify model, we have two tables and we either summarize / work in vertex-centric mode and join back to objects, or in object-centric mode and join back to vertices.

Since we have to have multiple tables (or matrices), why not allow them all to be full data frames with any data on them. If I plot or analyse with them, I can nominate that this is "x" and "y" geometrically, or "x", "y" and "time". The geometry is more flexible rather than be ing locked down on the object. The entities in vector Spatial data are objects, branches (or parts/pieces), and vertices. We can store attributes with the branches (am I a hole?) and we can also normalize to unique vertices if needed with a vertex-link branches table. Why? We need this for running triangulations on the vertices, or on providing shared-edit and other topologically constrained oeprations.

I think all of these options are important, and we should have easy tools for transferring between them.

convert Spatial to a sp\_df / tbl\_df with the Spatial column
-------------------------------------------------------------

``` r
#x <- sp_df(wrld_simpl)
#x %>% filter(NAME == "Australia")
```

Why do this?
------------

I want these things:

-   flexibility in the number and type/s of attribute stored as "coordinates", x, y, lon, lat, z, time, temperature, etc.
-   shared vertices
-   ability to store points, lines and areas together, sharing topology where appropriate

TODO
----

Create idioms for modifying the geometry with dplyr verbs and/or piping.

Consider whether `summarise` is a sensible for Spatial.

Can we do geometry manipulations like `sptable(w2) <- sptable(w2) %>% mutate(object = part, part = 1)` and sensibly restore the attributes on the way? Just copy the value outs?

Implement `sample_n` and `sample_frac`.

Implement the joins.

Approach
--------

Create methods for the dplyr verbs: filter, mutate, arrange, select etc.

This is part of an overall effort to normalize Spatial data in R, to create a system that can be stored in a database.

Functions `sptable` and `spFromTable` create `tbl_df`s from Spatial classes and round trip them. This is modelled on `raster::geom rather` than `ggplot2::fortify`, but perhaps only since I use raster constantly and ggplot2 barely ever.

Complicating factors are the rownames of sp and the requirement for them both on object IDs and data.frame rownames, and the sense in normalizing the geometry to the table of coordinates without copying attributes.

(See `mdsumner/gris` for normalizing further to unique vertices and storing parts and objects and vertices on different tables. Ultimately this should all be integrated into one consistent approach.)

Thanks to @holstius for prompting a relook under the hood of dplyr for where this should go: <https://gist.github.com/holstius/58818dc9bbb88968ec0b>

This package `spbabel` and these packages should be subsumed partly in the overall scheme:

<https://github.com/mdsumner/dplyrodbc> <https://github.com/mdsumner/manifoldr>
