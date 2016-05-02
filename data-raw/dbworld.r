library(maptools)
data("wrld_simpl")

library(dplyr)
library(RSQLite)
library(spbabel)

dbworld <- src_sqlite("inst/extdata/dbworld.sqlite3", create = TRUE)


vertices <- spbabel::sptable(wrld_simpl)
dat <- tbl_df(as.data.frame(wrld_simpl) ) %>% mutate(object_ = row_number())
vertices <- copy_to(dbworld, vertices, temporary = FALSE, indexes = list(
  c("object_", "branch_"), c("x_", "y_")))

dat <- copy_to(dbworld, dat, temporary = FALSE, indexes = list(
  "NAME"))


tbl(dbworld, "dat") %>% select(NAME, object_) %>% 
  filter(NAME %in% c("Australia", "New Zealand")) %>% 
  inner_join(tbl(dbworld, "vertices"))


anz <- 
  tbl(dbworld, "dat") %>% select(NAME, object_) %>% 
  filter(NAME %in% c("Australia", "New Zealand")) %>% 
  inner_join(tbl(dbworld, "vertices")) %>% 
  collect() %>% 
  spFromTable()

print(anz)

# rr <- readBin("inst/extdata/gworld.sqlite3", "raw", n = file.info("inst/extdata/gworld.sqlite3")$size)
# save(rr, file = "rr.rda", compress = "bzip2")


## gris version
gworld <- src_sqlite("inst/extdata/gworld.sqlite3", create = TRUE)
library(gris)
g <-gris::gris(wrld_simpl)
v <- g$v; v <- copy_to(gworld, v, name = "v", temporary = FALSE)
bXv <- g$bXv; bXv <- copy_to(gworld, bXv, name = "bXv", temporary = FALSE)
b <- g$b; b <- copy_to(gworld, b, name = "b", temporary = FALSE)
o <- g$o; o <- copy_to(gworld, o, name = "o", temporary = FALSE)

rm(v, bXv, b, o, gworld)

