\name{get_grid_increments}
\alias{get_grid_increments}
\alias{get_grid_increments.gribdata}
\title{Returns Latitude and Longitude Grid Increments from \code{gribdata} Object}
\description{
   Returns latitude and longitude grid increments of an object of class
   \code{gribdata} read by \code{\link{getgrib::getdata}}.
   Note: only valid for regular latlon grids (see \code{\link{get_grid_increments}}).
}
\usage{
   ## Usage
   get_grid_increments(x)
}
\arguments{
   \item{x}{Object of class \code{gribdata} from \code{getgrib::getdata}.}
}
\details{
   If the input object is not on a regular latlon grid the function
   stops program execution.

   If the data are identified as being on a regular latlon grid
   (see \code{\link{is_regular_ll_grid}}) a vector with two elements
   will be returned. First element is the grid increment on altitude,
   the second one the increment on longitude.
}
\examples{
   # Find package installation path including the demo grib file
   path <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   # Reading the '2t' 2m surface temperature forecast data
   gribdata <- getdata(path,'2t')

   # Convert into RasterStack object
   print(get_grid_increments(gribdata))
}
\seealso{
   \code{\link{getdata}},
   \code{\link{get_grid_increments}},
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
