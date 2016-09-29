
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------
library('getgrib')
file <- 'data/ECEPS_12.grib'
shortName <- '2t'

file <- 'data/hc.grib'
shortName <- 'tp'

test <- getdata(file,shortName)
rast <- gribdata2raster(test)
stop('dvstop')

   
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
   data <- cbind(Freturn[[3]],Freturn[[4]]); rm(Freturn)
   print(head(data[,1:6]))

   # ---------------------------------------------
   # Combine meta information and the values form the grib fields
   colnames(data) <- c('initdate','inithour','step','member',paste('gp',1:(ncol(data)-4),sep=''))

   # Development
   print(head(data[,1:6]))
   is_regular_ll_grid(dimension,lats,lons,TRUE)

   # Create raster data if possible
   if ( is_regular_ll_grid(dimension,lats,lons) ) {

      # Create layer names
      layernames <- sprintf("%06d%02d_%s_%03d_m%02d",
                     data[,'initdate'],data[,'inithour'],
                     shortName,data[,'step'],data[,'member'])
   
      # Cretae one empty grid
      increments <- get_grid_increments(dimension,lats, lons)
      require('raster')
      empty <- raster(nrows = dimension[2], ncols = dimension[1],
                      ymn   = min(lats) - increments[1]/2.,
                      ymx   = max(lats) + increments[1]/2.,
                      xmn   = min(lons) - increments[2]/2.,
                      xmx   = max(lons) + increments[2]/2.,
                      crs=CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
      # Create raster
      allRaster <- NULL
      cat(sprintf("Generating %d raster layers now\n",length(layernames)))
      pb <- txtProgressBar(0,length(layernames),style=3)
      for ( i in 1:nrow(data) ) {
         setTxtProgressBar(pb,i)
         tmp <- matrix(data[i,5:ncol(data)],nrow=dimension[1],ncol=dimension[2],byrow=F)
         tmp_raster <- empty; values(tmp_raster) <- t(tmp)
         names(tmp_raster) <- layernames[i]
         if ( is.null(allRaster) ) { allRaster <- tmp_raster; next }
         allRaster <- stack(allRaster,tmp_raster)
      }
      close(pb)
      require('colorspace')
      plot(allRaster,col=diverge_hcl(21))
      require('maps'); map(add=T)


   }












