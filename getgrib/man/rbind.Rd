\name{rbind}
\alias{rbind}
\alias{rbind.gribdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Combine \code{gribdata} Matrices}
\description{
   The function \code{\link{getdata}} returns the data from selected grib messages
   as \code{gribdata} objects. This method allows to combine these objects.

   Warning: only works for 'equivalent' \code{gribdata} objects where the original
   grids have had the same dimension, longitude/latitude definition, and shortName.
   If these attributes are differnet the \code{rbind} method stops.
}
\usage{
   ## Basic usage
   rbind( x, y )
}
\arguments{
   \item{x}{A \code{gribdata} object.}
   \item{...}{Further \code{gribdata} objects to be combined.}
}
\details{
   Combines \code{gribdata} objects (\code{rbind}) and adapts the
   required attributes of the object. Returns a new \code{gribdata}
   object containing all data.
}
\examples{
   # Find package installation path including the demo grib file
   file1 <- paste(path.package("getgrib"),"data/SnowSafeHindcast_201610100000.grib",sep="/")
   file2 <- paste(path.package("getgrib"),"data/SnowSafeHindcast_201610130000.grib",sep="/")

   # Loading file, creates gribdata object. As this is
   # total precipitation I already scale them from meters to
   # millimeters using the 'scale="*1000"' argument.
   gribdata1 <- getdata(file1,'tp',scale='* 1000')
   gribdata2 <- getdata(file2,'tp',scale='* 1000')

   # Combine data
   gribdata <- rbind(gribdata1,gribdata2)

   # Print size
   print(" Dimension of 'gribdata1':")
      print(dim(gribdata1))
   print(" Dimension of 'gribdata2':")
      print(dim(gribdata1))
   print(" Combined version:")
      print(dim(gribdata))

   # Create rasters
   g <- gribdata2raster(gribdata)
   plot(g)
}
\seealso{
   \code{\link{getdata}},
   \code{\link{deaccumulate}},
   \code{\link{gribdata2raster}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
