# -------------------------------------------------------------------
# - NAME:        SAMOS.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2016-09-30
# -------------------------------------------------------------------
# - DESCRIPTION: SAMOS steps, rather than creating a package now.
# -------------------------------------------------------------------
# - EDITORIAL:   2016-09-30, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2016-09-30 20:36 on thinkreto
# -------------------------------------------------------------------


# -------------------------------------------------------------------
# Do the samos stuff step by step
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Hindcast runs to consider:
# Returns an array with the most recent hindcasts which have
# to be considered to create the hindcast climatologies.
# -------------------------------------------------------------------
hindcastsToLoad <- function(date) {

   # If we are before Thusday: most recent is last Thursday
   if ( missing(date) ) date <- Sys.Date()
   date <- as.Date(date)

   # Try to find last available run
   dates <- data.frame("dates"=date + 14 - 0:7)
   dates$day <- strftime(dates$date,"%a")
   dates$diff <- dates$dates-Sys.Date()
   # Only taking Mondays and Thursdays
   dates <- dates[dates$day %in% c("Mon","Thu"),]
   # Last available hindcast run has to be:
   hcDate <- dates$date[which(dates$diff==max(dates$diff))]

   cat(sprintf(" - Last available hindcast run: %s\n",hcDate))

   # Which hindcast run's should be considered:
   # hcDate and the last 8 runs 
   dates <- data.frame("dates"= hcDate - 0:27,
                       "day"  = strftime(hcDate - 0:27,"%a"))
   dates <- dates[dates$day %in% c("Mon","Thu"),]
   cat(sprintf(" - Take the following hindcast runs for computation\n"))
   for ( i in 1:nrow(dates) ) {
      cat(sprintf(" - %s: %s\n",dates$day[i],dates$dates[i]))
   }

   return( dates$dates )
}
   
hcDates <- hindcastsToLoad()

hcDirectory <- "/home/retos/Downloads/operational_hindcasts"

# -------------------------------------------------------------------
# Loading hindcast data now
# -------------------------------------------------------------------
hindcastFiles <- function(date,dir,pattern="SnowSafeHindcast_tp_%Y%m%d_*") {
   date  <- as.Date(date)
   files <- list.files(dir,strftime(date,pattern))
   cat(sprintf(" - Looking for: %s\n",paste(dir,strftime(date,pattern),sep="/")))
   cat(sprintf(" - Found %2d files\n",length(files)))
   if ( length(files) == 0 ) return(NULL)
   return(paste(dir,files,sep="/"))
}


# -------------------------------------------------------------------
# Loading all hindcasts
# -------------------------------------------------------------------
require("getgrib")
library("raster")

   hcData <- list()
   cat(sprintf(" - Reading all hindcast data files\n"))
   for ( id in 1:length(hcDates) ) {
      # The date
      date <- hcDates[id]
      # List files
      files <- hindcastFiles(date,hcDirectory)
      cat(sprintf(" - Date %s: found %d files (File: %d/%d)\n",
               as.character(date),length(files),
               id,length(hcDates)))
      if ( length(files) == 0 ) next
      # Else read files and create raster
      for ( file in files ) {
         gribdata <- getdata(file,"tp",scale="* 1000")
         hcData[[length(hcData)+1]] <- gribdata2raster(deaccumulate(gribdata,
                                       setzero=TRUE,zeroval=0.01))
      }
   }

   library('colorspace')

   nx <- stack(hcData)
   plot(mean(nx),col=diverge_hcl(51))

   stop('dev stop')

   plot(nx,col=rainbow_hcl(51))









