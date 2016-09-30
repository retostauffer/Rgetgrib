\name{getdata}
\alias{getdata}
\alias{getdataByShortName}
\alias{getdataByMessageNumber}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracting Data from Grib Files}
\description{
   Written to directly extract data from grib files and import them
   into R. Returns an object of class \code{gribdata} which is mainly
   a \code{matrix} with some additional arguments required for further
   processing of the data.
}
\usage{
   ## Basic usage
   getdata( file, what )
}
\arguments{
   \item{file}{Character string. File name of the grib file to be concidered.}
   \item{what}{Selector what to load. Can eather be a \code{character} string
      or a \code{integer}. See details section.}
}
\details{
   Extracting grib data (full fields) directly from the grib file using
   the grib_api. Returns an object of class \code{gribdata} which has
   some additonal methods helping to process the data.

   Warning: as only one latitude/longitude vector is returned I currently  
   only allow to read grib files which unique grid definitions (same extent,
   same number of grid points/same dimension).

   There are two options for the input argument \code{what}. If \code{what}
   is a character this is interpreted as "shortName". All messages with this
   shortName will be returned. If \code{what} is an integer the message with
   the corresponding number will be returned.

   For each message one row will be returned, unique on initial date (dataDate),
   initial hour (dataTime), forecast step (step), and member (perturbationNumber).
   The first four columns of the object contain these meta information, while
   column \code{5} to \code{N} contain the data, grid point by grid point.

   Additional attributes are available where the most important ones are maybe:
   - lats: vector with the latitudes corresponding to the grid data (column
      \code{5} to \code{N})
   - lons: equivalent to \code{lats} longitudes.
}
\examples{
   # Find package installation path including the demo grib file
   file <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   # ------------------------------------------------------
   # Subsetting based on shortname
   # ------------------------------------------------------
   # Reading the '2t' 2m surface temperature forecast data
   gribdata_2t <- getdata(file,'2t') # getting all 2t forecasts
   gribdata_m5 <- getdata(file,5)    # getting message number 5

   # Convert into RasterStack object
   print(is_regular_ll_grid(gribdata_2t,TRUE))
   print(is_regular_ll_grid(gribdata_m5,TRUE))

   # Convert to raster and plot for demonstration
   plot( gribdata2raster(gribdata_2t) )
   plot( gribdata2raster(gribdata_m5) )

   # ------------------------------------------------------
   # Using grib_ls to specify what to get/plot.
   # ------------------------------------------------------
   # Let's assume that we would like to have 2m maximum temperature
   # of the demo ECEPS data file, 12 Z (there is only 12 in the file)
   idx <- grib_ls(file)
   idx <- which(idx$shortName=="mx2t" & idx$step==12 & idx$perturbationNumber==6)
   if ( ! length(idx) == 1 ) stop("Found more/less than exactly one message")
   # Else loading
   mx2t_member6 <- getdata(file,idx[1L])
   plot( gribdata2raster(mx2t_member6) )

   # Or total cloud cover
   idx <- grib_ls(file)
   idx <- which(idx$shortName=="tcc" & idx$step==12 & idx$perturbationNumber==6)
   if ( ! length(idx) == 1 ) stop("Found more/less than exactly one message")
   # Else loading
   tcc_member6 <- getdata(file,idx[1L])
   plot( gribdata2raster(tcc_member6) )
}
\seealso{
   \code{\link{gribdata2raster}},
   \code{\link{is_regular_ll_grid}},
   \code{\link{get_grid_increments}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
