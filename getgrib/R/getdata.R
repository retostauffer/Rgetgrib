# -------------------------------------------------------------------
# - NAME:        getdata.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-09-29
# -------------------------------------------------------------------
# - DESCRIPTION: Reading data from a grib file.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-09-29, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-10-02 12:06 on thinkreto
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Reading data from grib file
# -------------------------------------------------------------------
getdata <- function(file,what,scale) {
   if ( is.character(what) ) {
      return( getdataByShortName(file,what=what[1L],scale) )
   } else if ( is.numeric(what) || is.integer(what) ) {
      return( getdataByMessageNumber(file,what=what[1L],scale) )
   } else {
      stop("Unknown input to getgrib::getdata()")
   }
}


# -------------------------------------------------------------------
# Reading data from grib file, selector by shortName
# -------------------------------------------------------------------
getdataByShortName <- function(file,what,scale) {

   # Iput:
   shortName <- what

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
   nmembers       <- as.integer(Freturn[[2]][4])
   ninitdates     <- as.integer(Freturn[[2]][5])
   ninithours     <- as.integer(Freturn[[2]][6])
   ntotal <- as.integer(nsteps*nmembers*ninitdates*ninithours)

   # ---------------------------------------------
   # Getting longitude and latitude definition
   Freturn <- .Fortran('getgridll',file,as.integer(prod(dimension)),
                       as.numeric(rep(-9999,prod(dimension))),
                       as.numeric(rep(-9999,prod(dimension))),
                       PACKAGE='getgrib')
   lats <- Freturn[[3]]; lons <- Freturn[[4]]; rm(Freturn)

   # ---------------------------------------------
   # Getting data now
   Freturn <- .Fortran('getgriddataByShortName',file,shortName,
                       matrix(as.integer(-999),ntotal,4), # meta information
                       matrix(as.numeric(-999.),ntotal,prod(dimension)), # data (values)
                       as.integer(prod(dimension)), # number of grid points (col dimension)
                       ntotal, # steps times permutations (row dimension)
                       PACKAGE='getgrib')

   # ---------------------------------------------
   # If user sets 'scale': scale data. Can be any
   # vaid mathematical expression.
   if ( ! missing(scale) )
      eval(parse(text=sprintf("Freturn[[4]] <- Freturn[[4]] %s",scale)))

   # ---------------------------------------------
   # Combine meta information and the values form the grib fields
   data <- cbind(Freturn[[3]],Freturn[[4]]); rm(Freturn)
   colnames(data) <- c('initdate','inithour','step','member',paste('gp',1:(ncol(data)-4),sep=''))

   # ---------------------------------------------
   # Create vector of unique dates, hours, steps, and members
   steps     <- sort(unique(data[,'step']))
   initdates <- sort(unique(data[,'initdate']))
   inithours <- sort(unique(data[,'inithour']))
   members   <- sort(unique(data[,'member']))
   messagenumber <- NA

   # ---------------------------------------------
   # Create final object
   result <- list('data'=data,'lats'=lats,'lons'=lons,'dimension'=dimension)
   class(data) <- c('gribdata','matrix')
   keys <- c('shortName','dimension','lats','lons','file','initdates','ninitdates',
             'inithours','ninithours','steps','nsteps','members','nmembers','messagenumber')
   for ( key in keys ) eval(parse(text=sprintf("attr(data,'%s') <- %s",key,key)))
   return(data)
}

# -------------------------------------------------------------------
# Reading data from grib file. Only returns message number
# "messagenumber" (integer) 
# -------------------------------------------------------------------
getdataByMessageNumber <- function(file,what,scale) {

   # Input:
   messagenumber <- what

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

   # ---------------------------------------------
   # Getting data
   Freturn <- .Fortran('getgriddataByMessageNumber',file,as.integer(messagenumber),
                    paste(rep(" ",20),collapse=""),
                    rep(as.integer(-999),4), # meta information
                    rep(as.numeric(-999.),prod(dimension)), # data (values)
                    rep(as.numeric(-999.),prod(dimension)), # lats
                    rep(as.numeric(-999.),prod(dimension)), # lons
                    as.integer(prod(dimension)), # number of grid points (col dimension)
                    PACKAGE='getgrib')

   # ---------------------------------------------
   # If user sets 'scale': scale data. Can be any
   # vaid mathematical expression.
   if ( ! missing(scale) )
      eval(parse(text=sprintf("Freturn[[4]] <- Freturn[[4]] %s",scale)))

   # ---------------------------------------------
   # Create "gribdata" object
   # First combine meta information and data
   data <- t(c(Freturn[[4]],Freturn[[5]]))
   # Adding class and labels
   colnames(data) <- c("initdate","inithour","step","member",paste("gp",1:(ncol(data) - 4),sep = ""))
   class(data) <- c("gribdata","matrix")

   # ---------------------------------------------
   # Create vector of unique dates, hours, steps, and members
   shortName <- gsub(" ","",Freturn[[3]])
   lats      <- as.numeric(Freturn[[6]])
   lons      <- as.numeric(Freturn[[7]])
   steps     <- data[1,'step'];     nsteps     <- 1
   initdates <- data[1,'initdate']; ninitdates <- 1
   inithours <- data[1,'inithour']; ninithours <- 1
   members   <- data[1,'member'];   nmembers   <- 1

   # ---------------------------------------------
   # Create final object
   class(data) <- c('gribdata','matrix')
   keys <- c('shortName','dimension','lats','lons','file','initdates','ninitdates',
             'inithours','ninithours','steps','nsteps','members','nmembers','messagenumber')
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
get_grid_increments <- function(x) UseMethod('get_grid_increments')
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

   # Create raster data if possible
   if ( ! is_regular_ll_grid(x) ) stop("No regular ll grid, can't convert to raster.")

   # Create layer names
   layernames <- sprintf("%06d%02d_%s_%03d_m%02d",x[,'initdate'],x[,'inithour'],
                  attr(x,'shortName'),x[,'step'],x[,'member'])

   # Cretae one empty grid
   increments <- get_grid_increments(x)
   dimension <- attr(x,'dimension')
   lats      <- attr(x,'lats')
   lons      <- attr(x,'lons')
   empty <- raster::raster(nrows = dimension[2], ncols = dimension[1],
                   ymn   = min(lats) - increments[1]/2.,
                   ymx   = max(lats) + increments[1]/2.,
                   xmn   = min(lons) - increments[2]/2.,
                   xmx   = max(lons) + increments[2]/2.,
                   crs=sp::CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
   # Create raster
   cat(sprintf("Generating %d raster layers now\n",length(layernames)))
   pb <- txtProgressBar(0,length(layernames),style=3)
   allRaster <- list()
   allMeta <- as.data.frame(matrix(NA,nrow=ncol(x),ncol=4))
   names(allMeta) <- colnames(x)[1:4]
   for ( i in 1:nrow(x) ) {
      setTxtProgressBar(pb,i)
      tmp <- matrix(x[i,5:ncol(x)],nrow=dimension[1],ncol=dimension[2],byrow=F)
      tmp_raster <- empty; raster::values(tmp_raster) <- t(tmp)
      attr(tmp_raster,'meta') <- as.list(x[i,1:4])
      allMeta[i,1:4] <- x[i,1:4]
      names(tmp_raster) <- layernames[i]
      allRaster[[length(allRaster)+1]] <- tmp_raster
   }
   close(pb)

   # Stack raster list
   allRaster <- stack(allRaster)
   # Prepare and append meta information
   allMeta$initdate <- strptime(allMeta$initdate,"%Y%m%d")
   allMeta$valid <- allMeta$initdate + (allMeta$inithour + allMeta$step) * 3600
   attr(allRaster,'meta') <- allMeta

   # Return
   return(allRaster)
}


# -------------------------------------------------------------------
# Deaccumulate data from function 'getdata' (gribdata objects).
# -------------------------------------------------------------------
deaccumulate <- function(x,...) UseMethod('deaccumulate')
deaccumulate.gribdata <- function(x,deaccumulation=24,setzero=FALSE,zeroval=0.0001) {
   # Loading shared library
   library.dynam('getgrib',package='getgrib',lib.loc=.libPaths())

   # Rearange the data
   meta <- matrix(as.integer(x[,1:4]),ncol=4)
   data <- x[,5:ncol(x)]

   Fresult <- .Fortran('deaccumulate',meta,data,
                       rep(as.integer(0),nrow(data)), # success: returns 0/1 if deaccumulation was possible or not
                       as.integer(nrow(data)),as.integer(ncol(data)), # data dimension (and meta rows)
                       as.integer(deaccumulation), # hours to deaccumulate
                       as.integer(setzero), # setzero: set values below 0 to 0
                       as.numeric(zeroval), # values smaller set to this value (if setzero>0)
                       PACKAGE='getgrib')

   # Keep attributes, bind data
   hold <- attributes(x)
   x <- cbind(Fresult[[1]],Fresult[[2]])

   # Dropping 'non-deaccumulated' messages (rows)
   x <- x[which(Fresult[[3]]==1),]

   # Note: as we dropped the 'non deaccumulated' messages
   # we cannot add the dim attribute. Not required.
   for ( nam in names(hold) ) {
      if ( nam == 'dim' ) next
      attr(x,nam) <- hold[[nam]]
   }
   return(x)
}


# -------------------------------------------------------------------
# Cbind method - create S3 class 
# -------------------------------------------------------------------
#cbind <- function(...,deparse.level=1) UseMethod('cbind')
#cbind.default <- function(...,deparse.level=1) base::cbind(...,deparse.level) 
rbind.gribdata <- function(...,deparse.level=1) {

   # Getting input arguments
   x <- list(...)
   # If only one input: nothing to rbind. Return.
   if ( length(x) == 1 ) return(x[[1]])
   # Extracting attributes, reduce data class to 'matrix'
   attr <- list()
   for ( i in 1:length(x) ) {
      attr[[i]] <- attributes(x[[i]])
      class(x[[i]]) <- 'matrix' # Avoid rbind recursion
   }
   # Helper function
   retVal <- function(i,key) eval(parse(text=sprintf("attr[[%d]]$%s",i,key)))
   # Check if they are combinable
   # Matrix cols must be equivalent
   # $lon and $lat must be equivalent
   for ( i in 2:length(x) ) {
      if ( ! ncol(x[[1]]) == ncol(x[[i]]) ) stop("Matrix dimensons not equivalent. Stop.")
      # Shortname has to be the same
      if ( ! retVal(1,'shortName') == retVal(i,'shortName') ) stop("Different variables: different shortName attributes")
      # Check dimension
      if ( ! any(retVal(1,'dimension') == retVal(i,'dimension')) ) stop("Grid dimension different")
      # Longitude check
      if ( ! (length(retVal(1,'lons')) == length(retVal(i,'lons'))) ||
           ! all(retVal(1,'lons') == retVal(i,'lons')) ) stop("Different longitude definition")
      # Latitude check
      if ( ! (length(retVal(1,'lats')) == length(retVal(i,'lats'))) ||
           ! all(retVal(1,'lats') == retVal(i,'lats')) ) stop("Different latitude definition")
   }

   cmd <- sprintf("res <- rbind(%s)",paste(sprintf("x[[%d]]",1:length(x)),collapse=","))
   eval(parse(text=cmd))
   
   # Set class
   class(res) <- c('gribdata','matrix')

   # Now append attributes required for a proper 'gribdata' object
   attr(res,'shortName') = retVal(1,'shortName')
   attr(res,'dimension') = retVal(1,'dimension')
   attr(res,'lons') = retVal(1,'lons')
   attr(res,'lats') = retVal(1,'lats')
   attr(res,'messagenumber') <- NULL
   attr(res,'file') <- 'combined'
   keys <- c('initdates','inithours','steps','members')
   for ( k in keys ) {
      tmp <- retVal(1,k)
      for ( i in 2:length(x) ) {
         tmp <- sort(unique(c(tmp,retVal(i,k))))
      }
      attr(res,k) <- tmp
      attr(res,sprintf("n%s",k)) <- length(tmp)
   }

   a <<- attr
   # Now append attributes again
   return(res)

}















