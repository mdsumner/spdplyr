library(spbabel)
## normalized map
library(dplyr)
db <- src_sqlite(system.file("extdata", "gworld.sqlite3", package = "spbabel"))

aa <- tbl(db, "o") %>% filter(NAME == "New Zealand")
verts <- 
  aa %>% select(.ob0) %>% 
  inner_join(tbl(db, "b"), ".ob0") %>% 
  inner_join(tbl(db, "bXv"), ".br0") %>% 
  inner_join(tbl(db, "v", ".vx0")) %>% 
  arrange(.br0, .br_order)  %>%  collect() %>% filter(y < -39.3 & y > -45) %>% 
  transmute(x_ = x, y_ = y, object_ = .br0, branch_ = .br0, island_ = .h0)

plot(spFromTable(verts), col = sample(rainbow(length(unique(verts$object_)))))

## once we have the gris format, we only need the branches to figure out edges

vx <- aa %>% select(.ob0) %>% 
  inner_join(tbl(db, "b"), ".ob0") %>% 
  inner_join(tbl(db, "bXv"), ".br0") %>% 
  #inner_join(tbl(db, "v", ".vx0")) %>% 
  arrange(.br0, .br_order) %>% collect()

n2edge <- function(nx) {
  head(rep(1:2, nx) + rep(seq(nx)-1, each = 2), -2)
}
path2edge <- function(x)  x[n2edge(length(x))]

edgetab <- function(x) {
  closed <- path2edge(x); 
  closed <- c(closed, tail(closed, 1), head(closed, 1))
  data_frame(.vx0 = closed, .edg = rep(head(seq(length(x) + 1), -1), each = 2))
}


edge <- bind_rows(lapply(split(vx, vx$.br0), function(x) edgetab(x$.vx0)))

#vtx <- edge  %>% inner_join(vx)  %>% select(.vx0, .edg)  %>% inner_join(tbl(db, "v"), copy = TRUE)
## unique vertices
vtx <- tbl(db, "v") %>% left_join(edge, copy = TRUE) %>% filter(!is.na(.edg)) %>% select(x, y, .vx0) %>% collect()
vtx <- vtx %>% distinct(x, y) %>% mutate(rownum = row_number())
edge %>% inner_join(vtx) %>% select(rownum)
library(RTriangle)
ps <- pslg(P = vtx  %>% select(x, y) %>%  as.matrix(), 
           S = matrix((edge %>% inner_join(vtx) %>% select(rownum))$rownum, ncol = 2, byrow = TRUE))

plot(triangulate(ps, a = 1))
plot(ps)
