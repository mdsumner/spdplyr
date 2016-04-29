library(maptools)
data(wrld_simpl)
library(spbabel)
w <- wrld_simpl %>% filter(NAME %in% c("Australia",  "New Zealand"))
plot(w)
wtab <- sptable(w)
xy <- rgdal::project(wtab %>% dplyr::select(x_, y_) %>% as.matrix(), "+proj=laea +lon_0=160 +ellps=WGS84")
sptable(w) <- wtab %>% mutate(x_ = xy[,1], y_ = xy[,2])
proj4string(w) <- CRS("+proj=laea +lon_0=160 +lat_0=-40 +ellps=WGS84")
bufext <- function(e1, e2) {
  if (e2 == 0) return(e1)
  x <- e1     ## input object that has extent()
  chunk <- e2 ## rounding chunk
  num0 <- c(xmin(x), xmax(x), ymin(x), ymax(x))
  
  extent(    c((num0[c(1L, 3L)] %/% chunk) * chunk, 
               (num0[c(2L, 4L)] %/% chunk) * chunk + chunk)[c(1, 3, 2, 4)])
  
}
plot(w)
library(raster)
g <- rasterToPolygons(raster(bufext(w, 2e5), res = 2e5, crs = projection(w)), n = 4)
#g <- rasterToPolygons(raster(bufext(w, 5), res = 5, crs = projection(w)))

plot(g, add = TRUE)

gint <- intersect(w, g) 
gtab <- sptable(gint)
gtab$NAME <- as.character(gint$NAME[gtab$object_])
library(ggplot2)
ggplot(gtab) + aes(x_, y_, group = branch_, fill = NAME) + geom_polygon() + geom_path(col = "black")

ggplot(gtab) + aes(x_, y_, group = branch_, fill = object_) + geom_polygon()  + geom_path(col = "black")


reproj <- function (tbl, crs = NULL, cnames = c("x_", "y_")) 
{
  source <- 
  xy <- rgdal::project(as.matrix(dplyr::select_(.data, x, y)), crs)
  mutate_(.data, x_ = xy[,1], y_ = xy[,2])
}
