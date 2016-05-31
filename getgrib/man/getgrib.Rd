\name{getgrib}
\alias{getgrib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracting Data from Grib1 Files. Currently Nearest Neighbor}
\description{
   Originally written for Sascha. Extracting nearest neighbor data
   from a grib1 file using the ECMWF grib_api and a small fortran
   subfunction.
}
\usage{
   getgrib( x, lon, lat ) 
}
\arguments{
   \item{x}{File name of the grib1 file to be concidered.}
   \item{lon}{Numeric or Vector. List of longitude coordinates. Warning:
      for \code{rotated_ll} the rotated coordinates are required. See details.}
   \item{lat}{See \code{lon}}
}
\details{
   Extracting information from the grib1 file. For each \code{lon}, \code{lat}
   and each message in the grib file one nearest neighbor value will be returned.
   Please note: for \code{rotated_ll} grids (rotated lon/lat grids as provided
   by the COSMO) the ROTATED coordinates, not the TRUE geographical coordinates.

   Return values:
   - coords: \code{data.frame} containing original longitude and latitude values,
      and the longitude and latitude values of the nearest grid point.
   - params: \code{data.frame} with grib message meta information. Currently only
      \code{indicatorOfParameter}, \code{indicatorOfTypeOfLevel}, \code{level},
      \code{dataDate}, \code{dataTime}, \code{startStep}, and \code{endStep} will
      be extracted, all integers. These are those needed by Sascha to identify
      the correct message
   - data: a matrix of dimension \code{m} times \code{s} where \code{m} (rows)
      corresponds to the number of messages in the grib file, and also matches
      the rows in \code{params}. The columns contain the values for the different
      locations (matching \code{coords}) as requrested.
}
\examples{
   # Find package installation path including the demo grib file
   path <- paste(path.package("getgrib"),"data/COSMO2_surfaceheight.grb1",sep="/")

   # Path to demo file (included in package)
   print(path)

   # Extracting information
   # Note: rotated_ll around longitude -2/+1, and latitude -1/+1.
   # Therefore only runif coordinates are picked here.
   set.seed(100)
   res <- getgrib( path, runif(10,-2,2), runif(10,-1,1) )
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
