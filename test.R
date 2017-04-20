


library('getgrib')
file <- "/home/retos/Workingdirectory/snowpaper/station/newgrib/SnowPaperHindcast_pl_201701020000.grib"

   cat(sprintf(" * Interpolate data from file \"%s\"\n",file))
   listing <- grib_ls(file,c('shortName','typeOfLevel','level'))
   listing$shortName <- as.character(listing$shortName)
   listing$typeOfLevel <- as.character(listing$typeOfLevel)
   idx <- which(listing$typeOfLevel=="isobaricInhPa")
   if ( length(idx) > 0 )
      listing$shortName[idx] <- sprintf("%s%d",listing$shortName[idx],listing$level[idx])
   if ( missing(param) ) param <- as.character(unique(listing$shortName))

   idx <- which(listing$shortName == "z850")

   g <- getdata(file,head(idx))
   r <- gribdata2raster(g)




stop(' --- dev stop here --- ')


# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------
library('getgrib')
file1 <- paste(path.package("getgrib"),"data/ECMWF_t2m_demo.grib",sep="/")
file2 <- 'hc/SnowSafeHindcast_tp_20161010_2011101000.grib'

   
doit <- function( file ) {
   library.dynam('getgrib',package='getgrib',lib.loc=.libPaths())
   
   # Getting number of messages in the grib file. Needed to allocate
   # the corresponding results matrizes and vectors.
   cat(sprintf("   Calling .Fortran messagecount with %s\n",file))
   nmessages <- .Fortran('messagecount',file,as.integer(0),PACKAGE='getgrib')[[2]][1]
   
   # First we have to get the dimension of the grid. Note
   # that this function stops the script if not all grids
   # inside the grib file do have the same specification!
   cat(sprintf("   Calling .Fortran getgridinfo with %s\n",file))
   data <- .Fortran('getgridinfo',file,as.integer(rep(0,4)),PACKAGE='getgrib')
   print(data)

   dimension      <- data[[2]][1:2]
   nsteps         <- as.integer(data[[2]][3])
   nperturbations <- as.integer(data[[2]][4])

   cat(sprintf("   Calling .Fortran getgridll with %s\n",file))
   ll <- .Fortran('getgridll',file,as.integer(prod(dimension)),
            as.numeric(rep(-9999,prod(dimension))),
            as.numeric(rep(-9999,prod(dimension))),
            PACKAGE='getgrib')
   lats <- ll[[3]]; lons <- ll[[4]]; rm(ll)

   # Getting data now
   cat(sprintf("   Calling .Fortran getgriddata with %s\n",file))
   tmpF   <- .Fortran('getgriddata',file,
               matrix(as.integer(-999),as.integer(nsteps*nperturbations),2), # meta information
               matrix(as.numeric(-999.),nsteps*nperturbations,prod(dimension)), # data (values)
               as.integer(prod(dimension)), # number of grid points (col dimension)
               as.integer(nsteps*nperturbations), # steps times permutations (row dimension)
               PACKAGE='getgrib')
   metainfo <<- tmpF[[2]]
   values   <<- tmpF[[3]]

   return( cbind(metainfo,values) )
}

x <- doit(file1)
x <- doit(file2)

stop("dev stop")

# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------

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
