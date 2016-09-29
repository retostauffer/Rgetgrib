# -------------------------------------------------------------------
# - NAME:        grib_ls.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-09-29
# -------------------------------------------------------------------
# - DESCRIPTION: Interface to grib_ls. Using grib_ls lnx based.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-09-29, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-09-29 20:20 on thinkreto
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Well, that's all!
# -------------------------------------------------------------------
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
   return(data)
}

