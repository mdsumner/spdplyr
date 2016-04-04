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

names.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  names(x)
}
print.sp_df <- function(x, ...) {
  catclass <- class(x$Spatial)
  x$Spatial <- NULL
  
  cat(catclass, "\n")
  NextMethod("print", x)
}

as.data.frame.sp_df <- function(x, ...) {
  x$Spatial <- NULL
  NextMethod("as.data.frame", x)
}

plot.sp_df <- function(x, ...) {
  px <- as.data.frame(x)
  plot(SpatialPolygonsDataFrame(x$Spatial, px, match.ID = FALSE), ...)
}

`[.sp_df` <- function(x, i, j, ...) {
  id <- setNames(seq(nrow(x)), rownames(x))[i]
  sp <- x[["Spatial"]]
  #class(x) <- setdiff(class(x), "sp_df")
  d <- NextMethod("[", x)
  d$Spatial <- sp[id]
  class(d) <- c("sp_df", class(d))
  d
}

names.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("names", x)
}
dim.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("dim", x)
}
ncol.sp_df <- function(x) {
  x$Spatial <- NULL
  NextMethod("ncol", x)
}
as_Spatial.sp_df <- function(x, ...) {
  df <- as.data.frame(x)
  sp <- x$Spatial
  ## etc. etc. 
}
## convert Spatial to a sp_df / tbl_df with the Spatial column
x <- tSp(wrld_simpl)
x %>% filter(NAME == "Australia")



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

