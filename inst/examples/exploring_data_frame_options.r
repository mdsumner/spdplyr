library(dplyr)
library(nycflights13)
library(sp)


.serialize_Spatial0 <- function(x, format) {
  ## here we might use WKT as an option
  out <- NULL
  if (format == "wkb") {
    out <- wkb::writeWKB(x)
  }
  if (format == "wkt") {
    out <- rgeos::writeWKT(x)
  }
  class(out) <- c(format, class(out))
  out
}

.serialize_Spatial <- function(x, format = c("wkt", "wkb"), ...) {
  a <- vector("list", length(x))
  gx <- geometry(x)
  for (i in seq_along(a)) a[[i]] <-  .serialize_Spatial0(gx[1], format = format)

  a
}

print.wkb <- function(x, ...) {
  "(wkb)"
}
print.wkt <- function(x, ...) {
  "(wkt)"
}
c.wkt <- function(..., recursive = FALSE) {
  ac <- c(...)
  class(ac) <- c("wkt", "character")
}

`].wkt` <- function(x, i, ...) {
  a <- x[i]
  class(a) <- c("wkt", "character")
  a
}

show.wkt <- print.wkt
show.wkb <- print.wkb

wktify <- function(x, wkname = "wkt") {
  serial <- .serialize_Spatial(x, format = "wkt")
  x <- as.data.frame(x)
  x[[wkname]] <- unlist(serial)
  x
}

wkbify <- function(x, wkname = "wkb") {
  serial <- .serialize_Spatial(x, format = "wkb")
  x <- as.data.frame(x)
  x[[wkname]] <- serial
  x
}


wrld_simpl %>% wktify %>% summarize()

tSp <- function(x) {
  tab <- as_data_frame(as.data.frame(x))
  tab$Spatial <- geometry(wrld_simpl)
  class(tab) <- c("sp_df", class(tab))
  tab
}

x <- tSp(wrld_simpl)
print.sp_df <- function(x, ...) {
  x$Spatial <- sprintf("[%s %s]", class(x$Spatial), seq(nrow(x)))
  class(x) <- setdiff(class(x), "sp_df")
  print(x)
}

as.data.frame.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  class(x) <- setdiff(class(x), "sp_df")
  as.data.frame(x)
}
plot.sp_df <- function(x, ...) {
  px <- as.data.frame(x)
  plot(SpatialPolygonsDataFrame(x$Spatial, px, match.ID = FALSE), ...)
}




.dissolvers <- function(x) {
  switch(x, 
         union = rgeos::gUnion
         intersect = rgeos::gIntersection, 
         )
}
summarise_.Spatial <- function(.data, ..., dissolve = "union", wk_name = ".wk0") {
  if (!.hasSlot(.data, "data")) {
    stop("no data to summarise for a %s", class(.data))
  }
  dissolvefun <- .dissolvers(dissolve)
  pr4 <- proj4string(.data)
  dat <- as.data.frame(.data)
  #dat[[wk_name]] <- .serialize_Spatial(.data)
  dat <- summarise_(as.data.frame(.data), ...)
  if (is.grouped_dt(.data)) {
    gx <- geometry(.data)
    glist[[i]] <- gUnionCascaded(geometry(.data))
  }
  
}
group_by.Spatial <- function(.data, ...) {
  if (!.hasSlot(.data, "data")) {
        stop("no data to mutate for a %s", class(.data))
  }
  dat <- group_by_(as.data.frame(.data), .dots = dots)
  
}

