# setMethod("+", signature(e1='SpatialPolygons', e2='numeric'),
#           function(e1, e2){ 
#             spFromTable(sptable(e1) %>% mutate(x = x + e2, y = y + e2), crs = proj4string(e1))
#           }
# )


# setMethod("Arith", signature(e1='SpatialPolygonsDataFrame', e2='numeric'),
#           function(e1, e2){ 
#            tab <- sptable(e1)
#        
#             tab$x <- callGeneric(tab$x, e2)
#             tab$y <-callGeneric(tab$y, e2)
#             spFromTable(tab, crs = proj4string(e1))
#           }
# )
# setMethod("Arith", signature(e1='numeric', e2='SpatialPolygonsDataFrame'),
#           function(e1, e2){ 
#             tab <- sptable(e1)
#             tab$x <- callGeneric(tab$x, e1)
#             tab$y <- callGeneric(tab$y, e1)
#             spFromTable(tab, crs = proj4string(e1))
#           }
# )
