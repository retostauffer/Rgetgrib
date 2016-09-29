
# -----------------------------------------------------------------
# -----------------------------------------------------------------
# -----------------------------------------------------------------
library('getgrib')
file <- 'getgrib/data/ECEPS_12.grib'
shortName <- '2t'

#file <- 'data/hc.grib'
#shortName <- 'tp'

grib_ls <- function(file,parameters,where) {

   if ( ! file.exists(file) ) stop(sprintf("Sorry, file %s does not exist",file))
   # Base command
   cmd <- sprintf("grib_ls %s",file)
   # Adding selectors if rquired
   # Reto's defaults
   if ( missing(parameters) ) parameters <- "centre,dataDate,dataTime,perturbationNumber,shortName,step"
   if ( ! missing(parameters) ) {
      if ( is.character(parameters)) cmd <- sprintf("%s -p %s",cmd,paste(parameters,collapse=","))
   }
   if ( ! missing(where) ) {
      if ( is.list(where) ) cmd <- sprintf("%s -w %s",cmd,paste(names(where),where,sep="=",collapse=","))
      if ( is.character(where) ) cmd <- sprintf("%s -w %s",cmd,where)
   }
   # Show command
   cat(sprintf(" Calling: %s\n",cmd))
   tcon <- system(cmd,intern=TRUE)
   data <- read.table(textConnection(tcon),skip=1,nrows=length(tcon)-5,header=TRUE)

}

u <- grib_ls(file)
stop("dev stop")


u1 <- grib_ls(file)
u2 <- grib_ls(file,parameters="dataDate,dataTime,shortName,step")
u3 <- grib_ls(file,parameters=c("dataDate","dataTime","shortName","step"))

u4 <- grib_ls(file,where="shortName=2t,step=12")
u5 <- grib_ls(file,where=list("shortName"="cp","step"=12))


# Reading gribdata and raster them
message(" * Reading grib file, create gribdata object")
gribdata <- getdata(file,shortName)
message(" * Generate RasterStack object out of it")
rastered <- gribdata2raster(gribdata)


