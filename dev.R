
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------
library('getgrib')
file <- 'getgrib/data/ECEPS_12.grib'
shortName <- '2t'

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
   Freturn <- .Fortran('getgriddataByMessageNumber',file,as.integer(10),
                    paste(rep(" ",20),collapse=""),
                    rep(as.integer(-999),4), # meta information
                    rep(as.numeric(-999.),prod(dimension)), # data (values)
                    rep(as.numeric(-999.),prod(dimension)), # lats
                    rep(as.numeric(-999.),prod(dimension)), # lons
                    as.integer(prod(dimension)), # number of grid points (col dimension)
                    PACKAGE='getgrib')

   # ---------------------------------------------
   # Create "gribdata" object
   # First combine meta information and data
   data <- t(c(Freturn[[4]],Freturn[[5]]))
   # Adding class and labels
   colnames(data) <- c("initdate","inithour","step","member",paste("gp",1:(ncol(data) - 4),sep = ""))
   class(data) <- c("gribdata","matrix")

   # ---------------------------------------------
   # Create vector of unique dates, hours, steps, and members
   shortName <- trim(Freturn[[3]])
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
             'inithours','ninithours','steps','nsteps','members','nmembers')
   for ( key in keys ) eval(parse(text=sprintf("attr(data,'%s') <- %s",key,key)))
   
   print(Freturn[[3]])
   print(head(Freturn[[5]]))
   stop("x")


#file <- 'data/hc.grib'
#shortName <- 'tp'

#grib_ls <- function(file,parameters,where) {
#
#   if ( ! file.exists(file) ) stop(sprintf("Sorry, file %s does not exist",file))
#   # Base command
#   cmd <- sprintf("grib_ls %s",file)
#   # Adding selectors if rquired
#   # Reto's defaults
#   if ( missing(parameters) ) parameters <- "centre,dataDate,dataTime,perturbationNumber,shortName,step"
#   if ( ! missing(parameters) ) {
#      if ( is.character(parameters)) cmd <- sprintf("%s -p %s",cmd,paste(parameters,collapse=","))
#   }
#   if ( ! missing(where) ) {
#      if ( is.list(where) ) cmd <- sprintf("%s -w %s",cmd,paste(names(where),where,sep="=",collapse=","))
#      if ( is.character(where) ) cmd <- sprintf("%s -w %s",cmd,where)
#   }
#   # Show command
#   cat(sprintf(" Calling: %s\n",cmd))
#   tcon <- system(cmd,intern=TRUE)
#   data <- read.table(textConnection(tcon),skip=1,nrows=length(tcon)-5,header=TRUE)
#
#}

u1 <- grib_ls(file)
u2 <- grib_ls(file,parameters="dataDate,dataTime,shortName,step")
u3 <- grib_ls(file,parameters=c("dataDate","dataTime","shortName","step"))

u4 <- grib_ls(file,where="shortName=2t,step=12")
u5 <- grib_ls(file,where=list("shortName"="cp","step"=12))

for ( i in 1:5 ) eval(parse(text=sprintf('print(head(u%d))',i)))



