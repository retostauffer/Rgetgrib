\name{getdata}
\alias{getdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracting Data from Grib Files}
\description{
   Written to directly extract data from grib files and import them
   into R. Returns an object of class \code{gribdata} which is mainly
   a \code{matrix} with some additional arguments required for further
   processing of the data.
}
\usage{
   getdata( x, shortName )
}
\arguments{
   \item{x}{Character string. File name of the grib file to be concidered.}
   \item{shortName}{Character string. \code{shortName} of the variable
      corresponding to what is defined inside the grib file.}
}
\details{
   Extracting grib data (full fields) directly from the grib file using
   the grib_api. Returns an object of class \code{gribdata} which has
   some additonal methods helping to process the data.

   Warning: as only one latitude/longitude vector is returned I currently  
   only allow to read grib files which unique grid definitions (same extent,
   same number of grid points/same dimension).

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
   path <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   # Reading the '2t' 2m surface temperature forecast data
   gribdata <- getdata(path,'2t')

   # Convert into RasterStack object
   print(is_regular_ll_grid(gribdata,TRUE))
}
\seealso{
   \code{\link{gribdata2raster}},
   \code{\link{is_regular_ll_grid}},
   \code{\link{get_grid_increments}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
