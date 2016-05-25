


library('getgrib')
grbfile <- 'all.grib1'

lon <- runif(120,-2,2)
lat <- runif(120,-2,2)
system.time( x <- getgrib(grbfile,lon,lat) )


par(mfrow=c(1,2))

plot( x$coords$lon, x$coords$lat, cex=0.5 )
points(x$coords$closest_lon,x$coords$closest_lat,col=2,cex=1.0)

# Take some time series of .. temperatur
idx <- which(x$params$indicatorOfParameter==11 & x$params$level==2)

# Create time series of all stations
library('zoo')
init <- as.POSIXct(sprintf("%08d%04d",x$params$dataDate[idx],x$params$dataTime[idx]),format="%Y%m%d%H%M")
valid <- init + x$params$endStep[idx]*3600
z <- zoo( x$data[idx,], valid )
plot(z,screen=1)





###stop('dev exit')
###
###grbfile <- '/home/retos/Dropbox/Transfer-RetoJakob/COSMO2_surfaceheight.grb1'
###grbfile <- 'all.grib1'
###
###library('getgrib')
###library.dynam('getgrib',package='getgrib',lib.loc=.libPaths())
###
###nmessages <- .Fortran('messagecount',grbfile,as.integer(0))[[2]][1]
###cat(sprintf("  We will have %d messages in the flie\n",nmessages))
###
###
###lon <- runif(5,-2,2)
###lat <- runif(5,-2,2)
###
###nstations <- as.integer(length(lon))
###nmessages <- as.integer(nmessages)
###val <- matrix(-9999,ncol=nstations,nrow=nmessages)
#### Will be filed with paramId, date, time, startStep, endStep
###param <- matrix(as.integer(-9999),ncol=5,nrow=nmessages)
###
###cat(sprintf("  nstations: %6d\n",nstations))
###cat(sprintf("  nmessages: %6d\n",nmessages))
###print(dim(val))
###
###x <- .Fortran('getgrib', grbfile, nmessages, nstations, lon, lat, param, val ) 
###closest_lon  <- x[[4]]
###closest_lat  <- x[[5]]
###param        <- x[[6]]
###result       <- x[[7]]
###
###print(result)
###print(param)
