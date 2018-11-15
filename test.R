


library("getgrib")
library("sp")

stations <- SpatialPointsDataFrame(data.frame(lon = 11.2, lat = 47.6),
                data = data.frame(statnr = 666))

library("devtools")
load_all("getgrib")

gribfile <- "test.grb2"
gribfile <- "eceps.grib"
gribfile <- "/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703060000.grib"


x <- readgrib(gribfile)
print(str(x))

gribfiles <- c("/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703060000.grib",
               "/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703160000.grib")
x2 <- readgrib(gribfiles)
stop('---- devel ----')


print(" --------------------------- ")
gg <- getdata(gribfile) 
print(attr(gg, "shortName"))
print(dim(gg))

print(" --------------------------- ")
gg <- getdata(gribfile, "10u") 
print(attr(gg, "shortName"))
print(dim(gg))
stop('devel stop')

print(" --------------------------- ")
gg <- getdata(gribfile, "10u") 
print(attr(gg, "shortName"))
print(dim(gg))
stop()

print( bilinear(gribfile, stations))
