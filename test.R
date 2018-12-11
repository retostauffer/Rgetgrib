


library("getgrib")
library("sp")

stations <- SpatialPointsDataFrame(
                data.frame(lon = c(11.250, 11.2,-10), lat = c(47.625, 47.6,10)),
                data = data.frame(statnr = c("perfect", "inside", "outside")))

library("devtools")
load_all("getgrib")

gribfile <- "test.grb2"
gribfile <- "eceps.grib"
#gribfile <- "/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703060000.grib"

idw <- interpolate(gribfile, stations, method = "idw", k = 1, p = 1, corr.lat = F, verbose = 2)
stop('--devstop--')


bl <- interpolate(gribfile, stations, method = "bilinear") #, reshape = TRUE)
print(head(bl))
load_all("getgrib"); nn <- interpolate(gribfile, stations, method = "nearest", verbose = 2)
stop()



stop(" --- devel stop ----")
gribfiles <- c("/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703060000.grib",
               "/home/retos/Workingdirectory/SnowSAMOS/gribfiles/SnowPaperHindcast_201703160000.grib")

x2 <- readgrib(gribfiles, shortName = "2t", level = 0, steps = 12, check = FALSE)
print(dim(x2$data))
print(x2$step)
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
