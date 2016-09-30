\name{gribdata2raster}
\alias{gribdata2raster}
\alias{gribdata2raster.gribdata}
\title{Converting Prepared gribdata Objects to RasterStack}
\description{
   Converts objects of class \code{gribdata} read by 
   \code{\link{getdata}} into RasterStack objects if possible.
   Only if the imported data are recognized as regular latlon grids
   (not for rotated grids).
}
\usage{
   ## Usage
   gribdata2raster(x)
   ## Additional Arguments, but unused
   gribdata2raster(x,...)
}
\arguments{
   \item{x}{Object of class \code{gribdata} from \code{\link{getdata}}.}
   \item{...}{Currently not in use.}
}
\details{
   The function \code{getgrib::gribdata} returns an object of class \code{gribdata}
   which is actually a \code{matrix} with meta information (initial date and time
   of the run, forecast step, and member number) plus the corresponding data of
   the grib fields and several required attributes.

   This function (\code{gribdata2raster}) converts these data in a RasterStack
   object if possible. Currently only for regular latlon grids.

   Return values:
   - Object of class \code{RasterStack}. Each layer of the object contains
     the data of one specific grib field. 
   - The layers do have unique layer names and some additional attributes
     (see \code{attributes(x[[1]])}).
}
\examples{
   # Find package installation path including the demo grib file
   path <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   # Reading the '2t' 2m surface temperature forecast data
   gribdata <- getdata(path,'2t')

   # Convert into RasterStack object
   result <- gribdata2raster(gribdata)
   print(attr(result[[1]],'meta'))
   print(attr(result[[nlayers(result)]],'meta'))

   # Plotting
   plot(result,col=rainbow(21))
}
\seealso{
   \code{\link{getdata}},
   \code{\link{is_regular_ll_grid}},
   \code{\link{get_grid_increments}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
