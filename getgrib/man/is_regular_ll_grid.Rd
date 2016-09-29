\name{is_regular_ll_grid}
\alias{is_regular_ll_grid}
\alias{is_regular_ll_grid.gribdata}
\title{Checking if Input Object of Type \code{gribdata} is a Regular LatLon Grid}
\description{
   Checking weather the object of class \code{gribdata} read by 
   \code{getgrib::getdata} is a regular latlon grid.
}
\usage{
   ## Usage
   is_regular_ll_grid(x)
   ## Verbose
   is_regular_ll_grid(x,verbose=FALSE)
}
\arguments{
   \item{x}{Object of class \code{gribdata} from \code{getgrib::getdata}.}
   \item{verbose}{Boolean, default \code{FALSE}. If set to \code{TRUE} a message
      will be printed.}
   \item{...}{Currently not in use.}
}
\details{
   Checks if the unique latitude and unique longitude values correspond
   to the grid size (rows and columns). If so, the grid read was a regular
   latitude longitude grid. In this case \code{TRUE} will be returned,
   \code{FALSE} else.
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
   \code{\link{getdata}},
   \code{\link{gribdata2raster}},
   \code{\link{get_grid_increments}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
