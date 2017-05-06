library(testthat)
context("dplyr-Spatial")

## TODO: Rename context
## TODO: Add more tests

library(maptools)
library(spdplyr)
data(wrld_simpl)
poly1 <- wrld_simpl
line1 <- as(wrld_simpl, "SpatialLinesDataFrame")
point1 <- as(line1, "SpatialPointsDataFrame")
#mpoint1 <- SpatialMultiPointsDataFrame(lapply(split(line1, seq(nrow(line1))), function(y) coordinates(as(y, "SpatialPoints"))), 
#                                       as.data.frame(line1))

data(mpoint1)


test_that("filter works for all geometric types", {
  expect_that(nrow(filter(poly1, NAME == "Australia")), equals(1L) )
  expect_that(nrow(poly1 %>% filter(NAME == "Australia")), equals(1L) )
  
  expect_that(nrow(filter(line1, NAME == "Australia")), equals(1L) )
  expect_that(nrow(line1 %>% filter(NAME == "Australia")), equals(1L) )
  
  
  expect_that(nrow(filter(point1, NAME == "Australia")), equals(611L) )
  expect_that(nrow(point1 %>% filter(NAME == "Australia")), equals(611L) )
  
  expect_that(nrow(filter(mpoint1, NAME == "Australia")), equals(1L) )
  expect_that(nrow(mpoint1 %>% filter(NAME == "Australia")), equals(1L) )
  
  ## multipoint needs work, slicing here duplicates row.names so catch this warning as expected for now
  
  ## update 2017-04-30 MDS no warning needed anymore
  #expect_that(nrow(slice(mpoint1, 1:2)), testthat::gives_warning("some row.names duplicated"))
  expect_silent(nrow(slice(mpoint1, 1:2)))
})

# filter() (and slice())
# arrange()
# select() (and rename())
# distinct()
# summarise()
# sample_n() and sample_frac()

test_that("mutate works for all geometric types", {
  expect_that( ncol(mutate(poly1, NAME = NAME)), equals(11) )
  expect_that( ncol(mutate(line1, NAME = NAME)), equals(11) )
  expect_that( ncol(mutate(point1, NAME = NAME)), equals(14) )
  expect_that( ncol(mutate(mpoint1, NAME = NAME)), equals(11) )

  expect_that( ncol(mutate(poly1, NAMECOPY = NAME)), equals(12) )
  expect_that( ncol(mutate(line1, NAMECOPY = NAME)), equals(12) )
  expect_that( ncol(mutate(point1, NAMECOPY = NAME)), equals(15) )
  expect_that( ncol(mutate(mpoint1, NAMECOPY = NAME)), equals(12) )
  
  expect_that( ncol(transmute(poly1, NAMECOPY = NAME)), equals(1) )
  expect_that( ncol(transmute(line1, NAMECOPY = NAME)), equals(1) )
  expect_that( ncol(transmute(point1, NAMECOPY = NAME)), equals(1) )
  expect_that( ncol(transmute(mpoint1, NAMECOPY = NAME)), equals(1) )
})

test_that("arrange works", {
  expect_that(nrow(arrange(poly1)), equals(246L) )
  expect_that(nrow(arrange(point1)), equals(26264L) )
  expect_that(arrange(line1)$UN, equals(c(28L, 12L, 31L, 8L, 51L, 24L, 16L, 32L, 36L, 48L, 52L, 60L, 
                                          44L, 50L, 84L, 70L, 68L, 104L, 204L, 90L, 76L, 100L, 96L, 124L, 
                                          116L, 144L, 178L, 180L, 108L, 156L, 4L, 64L, 152L, 136L, 120L, 
                                          148L, 174L, 170L, 188L, 140L, 192L, 132L, 184L, 196L, 208L, 262L, 
                                          212L, 214L, 218L, 818L, 372L, 226L, 233L, 232L, 222L, 231L, 40L, 
                                          203L, 254L, 246L, 242L, 238L, 583L, 258L, 250L, 270L, 266L, 268L, 
                                          288L, 308L, 304L, 276L, 316L, 300L, 320L, 324L, 328L, 332L, 340L, 
                                          191L, 348L, 352L, 356L, 364L, 376L, 380L, 384L, 368L, 392L, 388L, 
                                          400L, 404L, 417L, 408L, 296L, 410L, 414L, 398L, 418L, 422L, 428L, 
                                          112L, 440L, 430L, 703L, 438L, 434L, 450L, 474L, 496L, 500L, 807L, 
                                          466L, 504L, 480L, 478L, 470L, 512L, 462L, 484L, 458L, 508L, 454L, 
                                          540L, 570L, 562L, 533L, 660L, 56L, 344L, 580L, 234L, 20L, 292L, 
                                          833L, 442L, 446L, 492L, 275L, 499L, 175L, 248L, 574L, 166L, 10L, 
                                          74L, 260L, 334L, 86L, 162L, 581L, 548L, 566L, 528L, 578L, 524L, 
                                          520L, 740L, 558L, 554L, 600L, 604L, 586L, 616L, 591L, 620L, 598L, 
                                          624L, 634L, 638L, 642L, 498L, 608L, 630L, 643L, 646L, 682L, 659L, 
                                          690L, 710L, 426L, 72L, 686L, 705L, 694L, 702L, 706L, 724L, 662L, 
                                          736L, 752L, 760L, 756L, 780L, 764L, 762L, 772L, 776L, 768L, 678L, 
                                          788L, 792L, 798L, 795L, 834L, 800L, 826L, 804L, 840L, 854L, 858L, 
                                          860L, 670L, 862L, 92L, 704L, 850L, 516L, 876L, 882L, 748L, 887L, 
                                          894L, 716L, 360L, 312L, 530L, 784L, 626L, 612L, 585L, 584L, 666L, 
                                          654L, 674L, 796L, 732L, 688L, 336L, 744L, 663L, 652L, 831L, 832L, 
                                          239L, 158L)))
  
  expect_that(arrange(poly1, NAME, UN)$SUBREGION[100], equals(30L))
})



test_that("select works", {
  expect_that(names(select_(poly1, "ISO2", "NAME")), equals(c("ISO2", "NAME")))
  expect_that(names(select_(line1, "ISO2", "NAME")), equals(c("ISO2", "NAME")))

})

# filter() (and slice())
# arrange()
# select() (and rename())
# distinct()
# mutate() (and transmute())
# summarise()
# sample_n() and sample_frac()

test_that("distinct works", {
          expect_that(nrow(distinct(poly1, REGION, .keep_all = TRUE)), equals(6L))
  expect_that(distinct(poly1, REGION, .keep_all = TRUE), is_a("SpatialPolygonsDataFrame"))
  
          })

test_that("mutate works", {
  expect_silent(mutate(poly1, NAME = "all same", AREA = row_number()))
  expect_silent(transmute(line1, Country= NAME, LON = 100))
  expect_that(transmute(line1, Country= NAME, LON = 100), is_a("SpatialLinesDataFrame"))
})


test_that("various examples", {
  testthat::skip_on_cran()
  expect_silent(wrld_simpl %>% mutate(NAME = "allthesame", REGION = row_number()))
  expect_silent(wrld_simpl %>% transmute(alpha = paste0(FIPS, NAME)) )
  expect_silent(wrld_simpl %>% filter(NAME %in% c("New Zealand", "Australia", "Fiji")))
  expect_silent(wrld_simpl %>% arrange(LON) )
  expect_silent(wrld_simpl %>% slice(c(9, 100)))
  expect_silent(wrld_simpl %>% dplyr::select(UN, FIPS))
  expect_silent(wrld_simpl %>% rename(`TM_WORLD_BORDERS_SIMPL0.2NAME` = NAME))
  expect_silent(wrld_simpl %>% distinct(REGION, .keep_all = TRUE) %>% arrange(REGION) )  ## first alphabetically in REGION
  expect_silent(wrld_simpl %>% arrange(REGION, desc(NAME)) %>% distinct(REGION, .keep_all = TRUE)) ## last
   
  ## we don't need to use piping
  expect_silent( slice(filter(mutate(wrld_simpl, likepiping = FALSE), abs(LON - 5) < 35 & LAT > 50), 4))
 ## summarise/ze is different, we have to return only one geometry
  expect_that(wrld_simpl %>% summarize(max(AREA)), is_a(class(wrld_simpl)))
  expect_silent(as(wrld_simpl, "SpatialLinesDataFrame") %>% mutate(perim = rnorm(246)))
  }
  
 )



ws <- filter(wrld_simpl, NAME %in% c("Portugal", "France", "Spain"))
ws$chNAME <- levels(ws$NAME)[ws$NAME]
d <- data.frame(ent = c("Australia", "Portugal", "New Zealand", "France", "Spain"), AREA = seq(1.2, 2, length = 5), 
                stringsAsFactors = FALSE)
test_that("joins work", {
  expect_that(all(is.na(left_join(ws@data, d)$ent)), is_true())
  expect_that(all(is.na(left_join(ws, d)$ent)), is_true())
  
  ## MDS changed 2017-05-06
  #expect_that(left_join(ws@data, d, c("NAME" = "ent")), gives_warning("joining character"))
  expect_that(left_join(ws@data, d, c("NAME" = "ent")), gives_warning("coercing into character vector"))
  expect_that(left_join(ws, d, c("chNAME" = "ent")), is_a(class(ws)))
  
  expect_that(nrow(inner_join(ws@data, d)), equals(0L))
  ## we can get 0-row S*DF
  expect_that(nrow(inner_join(ws, d)), equals(0L))
  
  expect_that(inner_join(ws@data, d, c("NAME" = "ent")), gives_warning("joining factor and character"))
  expect_that(inner_join(ws, d, c("chNAME" = "ent")), is_a(class(ws)))
  
  expect_that(nrow(inner_join(ws, d, c("chNAME" = "ent"))), equals(3L))
  
  expect_that(nrow(inner_join(ws, d[1:4, ], c("chNAME" = "ent"))), equals(2L))
  
})

