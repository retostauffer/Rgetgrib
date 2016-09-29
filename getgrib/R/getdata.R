# -------------------------------------------------------------------
# - NAME:        getdata.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-09-29
# -------------------------------------------------------------------
# - DESCRIPTION: Reading data from a grib file.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-09-29, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-09-29 13:58 on thinkreto
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Reading data from grib file
# -------------------------------------------------------------------
getdata <- function(file,shortName) {

   # Loading fortran library
   library.dynam('getgrib',package='getgrib',lib.loc=.libPaths())

   # ---------------------------------------------
   # Getting number of messages in the grib file. Needed to allocate
   # the corresponding results matrizes and vectors.
   nmessages <- .Fortran('messagecount',file,as.integer(0),PACKAGE='getgrib')[[2]][1]

   # ---------------------------------------------
   # First we have to get the dimension of the grid. Note
   # that this function stops the script if not all grids
   # inside the grib file do have the same specification!
   Freturn <- .Fortran('getgridinfo',file,as.integer(rep(0,6)),PACKAGE='getgrib')
   # Estracting required information
   dimension      <- Freturn[[2]][1:2]
   nsteps         <- as.integer(Freturn[[2]][3])
   nperturbations <- as.integer(Freturn[[2]][4])
   ndates         <- as.integer(Freturn[[2]][5])
   ntimes         <- as.integer(Freturn[[2]][6])
   ntotal <- as.integer(nsteps*nperturbations*ndates*ntimes)

   # ---------------------------------------------
   # Getting longitude and latitude definition
   Freturn <- .Fortran('getgridll',file,as.integer(prod(dimension)),
                       as.numeric(rep(-9999,prod(dimension))),
                       as.numeric(rep(-9999,prod(dimension))),
                       PACKAGE='getgrib')
   lats <- Freturn[[3]]; lons <- Freturn[[4]]; rm(Freturn)

   # ---------------------------------------------
   # Getting data now
   Freturn <- .Fortran('getgriddata',file,shortName,
                       matrix(as.integer(-999),ntotal,4), # meta information
                       matrix(as.numeric(-999.),ntotal,prod(dimension)), # data (values)
                       as.integer(prod(dimension)), # number of grid points (col dimension)
                       ntotal, # steps times permutations (row dimension)
                       PACKAGE='getgrib')

   # ---------------------------------------------
   # Combine meta information and the values form the grib fields
   data <- cbind(Freturn[[3]],Freturn[[4]]); rm(Freturn)
   colnames(data) <- c('initdate','inithour','step','member',paste('gp',1:(ncol(data)-4),sep=''))
   print(head(data[,1:6]))

   result <- list('data'=data,'lats'=lats,'lons'=lons,'dimension'=dimension)
   class(data) <- c('gribdata','matrix')
   keys <- c('shortName','dimension','lats','lons','file','ndates','ntimes','nsteps','nperturbations')
   for ( key in keys ) eval(parse(text=sprintf("attr(data,'%s') <- %s",key,key)))
   return(data)
}

# -------------------------------------------------------------------
# Helper function. We have loaded the grid dimension (number of
# rows and columns) from the grib file, stored in 'dimension',
# vector with two integer elements (ncol,nrow).
# Furthermore for each grid point longitude and latitude have
# been loaded. If the unique(longitude) and unique(latitude)
# length is equivalent to the 'dimension' argument we have a 
# regular longitude latitude grid. This is required for some
# further processing steps like e.g., creating a raster object
# from the data.
# @arg dimension.  vector with two elements. c(ncol,nrow)
# @arg lats. vector with latitudes corresponding to the gridpoints.
# @arg lons. vector with longitudes corresponding to the gridpoints.
# @arg verbose. If set, some output will be printed.
# @reutrn Returns boolean TRUE if the grid seems to be on a regular
#         longitude/latitude grid, FALSE else.
# -------------------------------------------------------------------
is_regular_ll_grid <- function(x,...) UseMethod('is_regular_ll_grid')
is_regular_ll_grid.gribdata <- function(x,verbose=FALSE) {
   dimension <- attr(x,'dimension')
   lats      <- attr(x,'lats')
   lons      <- attr(x,'lons')
   ll <- c(length(unique(lons)),length(unique(lats)))
   if ( verbose ) {
      if ( all(ll == dimension) ) {
         message("Loaded data seem to be on regular latlons grid.")
      } else {
         message("Loaded data seem to be on a rotated or non-ll grid.")
      }
   }
   return( all(ll == dimension) )
}

# -------------------------------------------------------------------
# Computes grid increments in longitude and latitude direction.
# @arg dimension.  vector with two elements. c(ncol,nrow)
# @arg lats. vector with latitudes corresponding to the gridpoints.
# @arg lons. vector with longitudes corresponding to the gridpoints.
# @reutrn Returns vector with two elements. First element is
#         increment in latitude direction, second one in longitude
#         direction.
# -------------------------------------------------------------------
get_grid_increments <- function(x,...) UseMethod('get_grid_increments')
get_grid_increments.gribdata <- function(x) {
   dimension <- attr(x,'dimension')
   lats      <- attr(x,'lats')
   lons      <- attr(x,'lons')
   if ( ! is_regular_ll_grid(x) )
      stop("Sorry, no regular ll grid. Cannot compute grid increments!")
   # Latitude increment
   delta_lat <- diff(sort(unique(lats)))
   if ( length(unique(delta_lat)) != 1 )
      stop("No uniform grid spacing in latitude direction! Stop.")
   # Longitude increment
   delta_lon <- diff(sort(unique(lons)))
   if ( length(unique(delta_lon)) != 1 )
      stop("No uniform grid spacing in longitude direction! Stop.")
   # Else return
   return( c(unique(delta_lat),unique(delta_lon)) )
}


# -------------------------------------------------------------------
# Convert gribdata (read by getdata) into raster objects, if
# possible.
# -------------------------------------------------------------------
gribdata2raster <- function(x,...) UseMethod('gribdata2raster')
gribdata2raster.gribdata <- function(x,...) {

   require('raster')

   # Create raster data if possible
   if ( ! is_regular_ll_grid(x) ) stop("No regular ll grid, can't convert to raster.")

   # Create layer names
   layernames <- sprintf("%06d%02d_%s_%03d_m%02d",x[,'initdate'],x[,'inithour'],
                  attr(x,'shortName'),x[,'step'],x[,'member'])

   # Cretae one empty grid
   print(class(x))
   increments <- get_grid_increments(x)
   dimension <- attr(x,'dimension')
   lats      <- attr(x,'lats')
   lons      <- attr(x,'lons')
   empty <- raster(nrows = dimension[2], ncols = dimension[1],
                   ymn   = min(lats) - increments[1]/2.,
                   ymx   = max(lats) + increments[1]/2.,
                   xmn   = min(lons) - increments[2]/2.,
                   xmx   = max(lons) + increments[2]/2.,
                   crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
   # Create raster
   cat(sprintf("Generating %d raster layers now\n",length(layernames)))
   pb <- txtProgressBar(0,length(layernames),style=3)
   allRaster <- list()
   for ( i in 1:nrow(x) ) {
      setTxtProgressBar(pb,i)
      tmp <- matrix(x[i,5:ncol(x)],nrow=dimension[1],ncol=dimension[2],byrow=F)
      tmp_raster <- empty; values(tmp_raster) <- t(tmp)
      attr(tmp_raster,'meta') <- as.list(x[i,1:4])
      names(tmp_raster) <- layernames[i]
      allRaster[[length(allRaster)+1]] <- tmp_raster
   }
   close(pb)

   # Stack raster list
   allRaster <- stack(allRaster)
   return(allRaster)

}


















