# -------------------------------------------------------------------
# - NAME:        getnearest.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-05-25
# -------------------------------------------------------------------
# - DESCRIPTION: Calling getnearest fortran routine to extract data
#                and values from a grib file.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-05-25, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-10-18 13:45 on pc24-c707
# -------------------------------------------------------------------


getnearest <- function(file, lon, lat, tozoo=FALSE ) {

   if ( ! length(file) == 1 ) stop("Input argument 'file' has to be length 1!")
   if ( ! file.exists(file) ) stop(sprintf("Sorry, but file %s does not exist",file))

   # If length of lon/lat is not equal
   if ( ! length(lon) == length(lat) ) stop("Length of in put 'lon', 'lat' not equal")
   if ( length(lon) == 0 ) stop("Input 'lon', 'lat' cannot be of length zero")

   # Loading fortran library 
   library.dynam('getgrib',package='getgrib',lib.loc=.libPaths())

   # Getting number of messages in the grib file. Needed to allocate
   # the corresponding results matrizes and vectors.
   nmessages <- .Fortran('messagecount',file,as.integer(0),PACKAGE='getgrib')[[2]][1]
   cat(sprintf("  We will have %d messages in the file\n",nmessages))

   # Prepare input arguments for the fortran routine
   nstations <- as.integer(length(lon))
   nmessages <- as.integer(nmessages)
   val <- matrix(-9999,ncol=nstations,nrow=nmessages)
   # Will be filed with indicatorOfParameter, indicatorOfTypeOfLevel, level, date, time, startStep, endStep
   param <- matrix(as.integer(-9999),ncol=7,nrow=nmessages)

   # Now calling fortran code
   x <- .Fortran('getnearest', file, nmessages, nstations, lon, lat, param, val, PACKAGE='getgrib' ) 
   closest_lon  <- x[[4]]
   closest_lat  <- x[[5]]
   param        <- x[[6]]
   data         <- x[[7]]

   # Prepare results matrix
   coords <- data.frame("lon"=lon,"lat"=lat,"closest_lon"=closest_lon,"closest_lat"=closest_lat)
   # Prepare parameter information
   params <- as.data.frame(param)
   names(params) <- c('indicatorOfParameter','indicatorOfTypeOfLevel','level',
                      'dataDate','dataTime','startStep','endStep')


   # Try to convert to zoo, only if there is one singel
   # variable in the grib file and only one station has been
   # requested. If not possible to convert
   # simply set "tozoo" to "FALSE", wherefore the list object
   # will be returned. 
   if ( tozoo ) {
      if ( ! nrow(coords) == 1 ) {
         warning("Cannot create zoo object as multiple stations have been requested. Return list.")
         tozoo <- FALSE
      } else if ( ! nrow(unique(params[,1:3])) == 1 ) {
         warning("Cannot create zoo as there are different variables in the loaded data set. Return list object!")
         tozoo <- FALSE
      }
   }

   # If tozoo is still TRUE create the zoo object
   if ( tozoo ) {
      require("zoo")
      idx <- strptime(sprintf("%08d %04d",params$dataDate,params$dataTime),"%Y%m%d %H%M")
      idx <- as.POSIXlt(idx) + params$startStep * 3600
      if ( ! sum(params$startStep-params$endStep) == 0 ) {
         data <- zoo( cbind(subset(params,select=c(startStep,endStep)),data), idx )
      } else {
         data <- zoo( data, idx )
      }
      attr(data,"coords") <- coords
      return(data)
   }

   # Else return the list object
   return( list('coords'=coords,'params'=params,'data'=data) )
}













