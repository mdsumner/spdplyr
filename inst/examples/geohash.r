##devtools::install_github("mdsumner/spbabel")
##devtools::install_github("mdsumner/gris", ref = "cran-sprint")
library(spbabel)
library(geohash)
library(maptools); data(wrld_simpl)
library(pryr)
library(gris)
library(ggplot2)

## how big is wrld_simpl?

## entity tables, nested (not normalized)
dbtab <- spbabel::db_df(wrld_simpl)
## object table->nested_branch->nested_vertices (not normalized)
sptab <- spbabel::sp_df(wrld_simpl)
## normalized tables
grtab <- gris::gris(wrld_simpl)

## basic object table
d <- as.data.frame(wrld_simpl)
## basic gg table
gg <- ggplot2::fortify(wrld_simpl)


object_size(wrld_simpl)
object_size(list(d, gg))
object_size(dbtab)
object_size(sptab)
object_size(grtab)

## even smaller
library(dplyr)

dbhash <- dbtab
dbhash$Table$Coord$gh <- geohash::gh_encode(dbhash$Table$Coord$y, dbhash$Table$Coord$x, 6)
dbhash$Table$Coord <- dbhash$Table$Coord %>% dplyr::select(branch, gh)
object_size(dbhash)

## can we restore from dbtab? 
Objects <- function(x) x$Table$Object
Branches <- function(x) x$Table$Branch
Coords <- function(x) x$Table$Coord

xy <- geohash::gh_decode(dbhash$Table$Coord$gh)
dbhash$Table$Coord$x <- xy$lng
dbhash$Table$Coord$y <- xy$lat
dbhash$Table$Coord$gh <- NULL
countries <- Objects(dbhash) %>% inner_join(Branches(dbhash)) %>% inner_join(Coords(dbhash))
sprestore <- spFromTable(countries, attr = Objects(dbhash), quiet = TRUE)
plot(sprestore)
text(coordinates(sprestore), lab = sprestore$NAME)

print(sprestore)


## can we restore from dbtab? 
library(dplyr)
Objects <- function(x) x$Table$Object
Branches <- function(x) x$Table$Branch
Coords <- function(x) x$Table$Coord

countries <- Objects(dbtab) %>% inner_join(Branches(dbtab)) %>% inner_join(Coords(dbtab))
sprestore <- spFromTable(countries, attr = Objects(dbtab), quiet = TRUE)
plot(sprestore)
text(coordinates(sprestore), lab = sprestore$NAME)

print(sprestore)
