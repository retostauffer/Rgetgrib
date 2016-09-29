


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
   print(system.time(
      data <- .Fortran('getgridinfo',file,as.integer(rep(0,4)),PACKAGE='getgrib')
   ))

   dimension      <- data[[2]][1:2]
   nsteps         <- as.integer(data[[2]][3])
   nperturbations <- as.integer(data[[2]][4])

   cat(sprintf("   Calling .Fortran getgridll with %s\n",file))
   print(system.time(
      ll <- .Fortran('getgridll',file,as.integer(prod(dimension)),
               as.numeric(rep(-9999,prod(dimension))),
               as.numeric(rep(-9999,prod(dimension))),
               PACKAGE='getgrib')
   ))
   lats <- ll[[3]]; lons <- ll[[4]]; rm(ll)

   # Getting data now
   cat(sprintf("   Calling .Fortran getgriddata with %s\n",file))
   print(system.time(
      tmpF   <- .Fortran('getgriddata',file,
                  matrix(as.integer(-999),as.integer(nsteps*nperturbations),2), # meta information
                  matrix(as.numeric(-999.),nsteps*nperturbations,prod(dimension)), # data (values)
                  as.integer(prod(dimension)), # number of grid points (col dimension)
                  as.integer(nsteps*nperturbations), # steps times permutations (row dimension)
                  PACKAGE='getgrib')
   )) 
   metainfo <<- tmpF[[2]]
   values   <<- tmpF[[3]]

   return( cbind(metainfo,values) )
}

t1 <- system.time( x <- doit(file1) )
t2 <- system.time( x <- doit(file2) )

cat('\n===============================')
cat('\n---------- t1\n')
print(t1)
cat('\n---------- t2\n')
print(t2)
