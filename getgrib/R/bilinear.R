# -------------------------------------------------------------------
# - NAME:        bilinear.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-04-22
# -------------------------------------------------------------------
# - DESCRIPTION: Performs multi-station bilinear interpolation on
#                grib files. Currently using two things:
#                - call getgrib::grib_ls to get inventory
#                - call bilinear C-function to interpolate
#                - creates return values
#
#                Function in alpha-state, kind of tuned to 
#                interpolate ECMWF ensemble data on regular_ll!
# -------------------------------------------------------------------
# - EDITORIAL:   2017-04-22, RS: Created file on thinkreto.
#                2017-04-23, RS: NA handling for stations outside grid
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2017-10-23 17:08 on thinkreto
# -------------------------------------------------------------------

bilinear <- function(file,stations,verbose=FALSE,reshape=FALSE) {

   # Check input
   if ( ! is.character(file) )
      stop("Input 'file' has to be a character string.")
   if ( ! file.exists(file) )
      stop(sprintf("Cannot find file \"%s\"",file))
   if ( ! class(stations) == "SpatialPointsDataFrame" )
      stop("Input 'stations' must be of type SpatialPointsDataFrame")
   if ( ! 'statnr' %in% names(stations) )
      stop("Input 'stations' must contain stations$statnr (integer).")

   if ( class(stations$statnr) == "character" ) {
      stations$statnr <- factor(stations$statnr)
   }
      
   # Perform bilinear interpolation
   res <- .Call("grib_bilinear_interpolation",
                as.character(file),
                as.numeric(stations$statnr),
                stations@coords[,"lon"],stations@coords[,"lat"],
                as.integer(verbose), PACKAGE="getgrib")

   colnames(res$meta) <- c("init","runhour","step","member")

   res$meta           <- as.data.frame(res$meta)
   if ( is.numeric(stations$statnr) ) {
      colnames(res$data) <- sprintf("station_%s", as.character(stations$statnr))
   } else {
      colnames(res$data) <- sprintf("%s", as.character(stations$statnr))
   }

   # Replace missing values with NA
   idx <- which(res$data == -9999., arr.ind=TRUE)
   if ( nrow(idx) > 0 ) res$data[idx] <- NA

   # compute init date and valid date and add as POSIXct
   res$meta$init    <- strptime(res$meta$init,"%Y%m%d") + res$meta$runhour*3600
   res$meta$runhour <- res$meta$init + res$meta$step * 3600
   names(res$meta)[which(names(res$meta)=="runhour")] <- "valid"

   # Manipulate shortnames, combine data
   shortNames <- manipulate_shortnames(res$shortName,res$level,res$typeOfLevel)
   data <- as.data.frame(cbind(res$meta,shortNames,res$data),stringsAsFactor=F)

   # no reshaping requested
   if ( ! reshape ) {
      attr(data,'shortName')       <- res$shortName
      attr(data,'level')           <- res$level
      attr(data,'typeOfLevel')     <- res$typeOfLevel
      attr(data,'created')         <- Sys.time()
      attr(data,'getgrib_version') <- packageVersion("getgrib")
      return(data)
   }
   
   # If reshape is requested: reshape station-by-station and
   # store everything in a list object 'res'
   reslist <- list()
   for ( i in 1:nrow(stations) ) {
      hash <- sprintf("station_%d",stations$statnr[i])
      take <- c(which(!grepl("^station_[0-9]{1,}$",names(data))),which(names(data)==hash))
      tmp <- reshape(data[,take], timevar='member',
                     idvar=c('init','valid','step','shortName'), direction='wide')
      names(tmp) <- gsub(sprintf("%s.",hash),"member_",names(tmp))
      # Sort
      take <- c(which(!grepl("^member_[0-9]{1,}$",names(tmp))),
                match(sprintf("member_%d",sort(unique(data$member))),names(tmp) ))
      reslist[[hash]] <- tmp[,take]
   }
   attr(reslist,'shortName')       <- res$shortName
   attr(reslist,'level')           <- res$level
   attr(reslist,'typeOfLevel')     <- res$typeOfLevel
   attr(reslist,'created')         <- Sys.time()
   attr(reslist,'getgrib_version') <- packageVersion("getgrib")
   return( reslist )
}

# Manipulate variable names
manipulate_shortnames <- function( shortName, level, typeOfLevel ) {

   # Manipulte t2 and d2
   idx <- which( grepl("^2t$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "t2m"
   idx <- which( grepl("^2d$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "d2m"
   # Same with 100u and 100v
   idx <- which( grepl("^100u$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "u100"
   idx <- which( grepl("^100v$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "v100"

   # For variables on isobaricInHectopascal add level 
   idx <- which( grepl("^isobaricInhPa$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s%d",shortName[idx],level[idx])

   # For hybrid levels (model level data)
   idx <- which( grepl("^hybrid$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s_%d",shortName[idx],level[idx])

   return( matrix(shortName,ncol=1,dimnames=list(NULL,'shortName')) )

}
















