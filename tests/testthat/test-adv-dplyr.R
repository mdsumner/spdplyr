library(testthat)
library(dplyr)
context("adv-dplyr")


library(maptools)
library(dplyr)
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")
test_that("group by and summarize is quiet", {
  expect_silent(g <- wrld_simpl  %>% group_by(REGION) )
  expect_silent(g[1:20, ] %>% summarize(alon = mean(LON), mxlat = max(LAT), mxarea = max(AREA)))
  expect_silent(wrld_simpl[1:20, ]  %>% group_by(REGION, SUBREGION) %>% summarize(alon = mean(LON), mxlat = max(LAT), mxarea = max(AREA)))
  expect_silent(g[1:10, ] %>% summarise(alon = mean(LON)))
  
  ## make sure we are &ing togeter in filter
  worldcorner <- wrld_simpl %>% 
    mutate(lon = coordinates(wrld_simpl)[,1], lat = coordinates(wrld_simpl)[,2]) %>% 
    filter(lat < -20, lon > 60) %>% 
    dplyr::select(NAME)
  
  
})


#mpoint1 <- SpatialMultiPointsDataFrame(lapply(split(line1, seq(nrow(line1))), function(y) coordinates(as(y, "SpatialPoints"))), 
#                                       as.data.frame(line1))
# filter() (and slice())
# arrange()
# select() (and rename())
# distinct()
# mutate() (and transmute())
# summarise()
# sample_n() and sample_frac()
# 
# group_by and summarize
# 
# inner_join()	SELECT * FROM x JOIN y ON x.a = y.a
# left_join()	SELECT * FROM x LEFT JOIN y ON x.a = y.a
# right_join()	SELECT * FROM x RIGHT JOIN y ON x.a = y.a
# full_join()	SELECT * FROM x FULL JOIN y ON x.a = y.a
# semi_join()	SELECT * FROM x WHERE EXISTS (SELECT 1 FROM y WHERE x.a = y.a)
# anti_join()	SELECT * FROM x WHERE NOT EXISTS (SELECT 1 FROM y WHERE x.a = y.a)
# intersect(x, y)	SELECT * FROM x INTERSECT SELECT * FROM y
# union(x, y)	SELECT * FROM x UNION SELECT * FROM y
# setdiff(x, y)	SELECT * FROM x EXCEPT SELECT * FROM y
# 

test_that("everthing is ok", {
  testthat::skip_on_cran()
  x1 <- poly1
  
  x1 %>% filter(NAME == "Australia")
  x1 %>% filter(POP2005 > 2e6)
  x1 %>% filter(grepl("Aust", NAME))
  x1 %>% slice(c(100, 200))
  
  x1 %>% arrange(AREA)
  x1 %>% dplyr::select(NAME, POP2005, AREA)
  x1 %>% rename(Country = NAME, Population = POP2005)
  expect_that(x1 %>% distinct(AREA, .keep_all = TRUE) %>% nrow(), equals(204))
  x1 %>% mutate(AREA = REGION * 2)
  x1 %>% transmute(NAME = gsub("^A", "Z", NAME))
  x1[sample(nrow(x1), 10), ] %>% summarize(a = 'POP2005')
  
  #x1 %>% group_by(REGION)
  #x1 %>% summarize(a = POP2005)
})

test_that("mutate_all, mutate_at", {
  skip_if_not(utils::packageVersion("dplyr") > "0.5.0")

library("maptools")
library("spdplyr")

data(wrld_simpl)

wrld_simpl %>% mutate_all(funs(as.character))
wrld_simpl %>% mutate_at(vars(starts_with("L")), funs(as.integer))
#wrld_simpl %>% mutate_if(vars(starts_with("L")), funs(as.integer))
 spmap %>% mutate_if(is.numeric, as.character)
 spmap %>% mutate_all(funs(as.character))
 spmap %>% mutate_at(vars(starts_with("L")), funs(as.integer))

})

