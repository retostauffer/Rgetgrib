


library("getgrib")
library("sp")

stations <- SpatialPointsDataFrame(data.frame(lon = 11.2, lat = 47.6),
                data = data.frame(statnr = 666))


gribfile <- "test.grb2"

print( bilinear(gribfile, stations))
