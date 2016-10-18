\name{getnearest}
\alias{getnearest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extracting Data from Grib1 Files. Currently Nearest Neighbor}
\description{
   Originally written for Sascha. Extracting nearest neighbor data
   from a grib1 file using the ECMWF grib_api and a small fortran
   subfunction.
}
\usage{
   getnearest( file, lon, lat, tozoo = FALSE ) 
}
\arguments{
   \item{file}{File name of the grib1 file to be concidered.}
   \item{lon}{Numeric or Vector. List of longitude coordinates. Warning:
      for \code{rotated_ll} the rotated coordinates are required. See details.}
   \item{lat}{See \code{lon}}
   \item{tozoo}{logical, default \code{FALSE}. If set to \code{TRUE} a zoo object
      will be returned if possible. See details.}
}
\details{
   Extracting information from the grib1 file. For each \code{lon}, \code{lat}
   and each message in the grib file one nearest neighbor value will be returned.
   Please note: for \code{rotated_ll} grids (rotated lon/lat grids as provided
   by the COSMO) the ROTATED coordinates, not the TRUE geographical coordinates.

   Return values for \code{tozoo=FALSE} or if conversion to \code{zoo} not possible:
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

   Return values for \code{tozoo=TRUE}:
   If \code{tozoo} is set to \code{TRUE} the method tries to convert the data
   extracted from the grib file into a zoo object. This is only possible if
   (i) only one single station was extracted (only one value fur \code{lon} and
   \code{lat}) and if only one variable has been found in the grib file.
   In case startStep and endStep in the grib file differ for the extracted
   variable a multivariate \code{zoo} will be returned including:
   - startStep, endStep, and data whereas the last column contains the
     extracted data.
   If startStep and endStep in the grib meta are identical only an univariate
   \code{zoo} object will be returned containing the extracted data.
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
   res <- getnearest( path, runif(10,-2,2), runif(10,-1,1) )
   # Loading data set
   # This is - as an example - the ECMWF HIRS 2m temperature
   # forecast of a certain date for step +0, +1, and +2 hours. 
   path <- paste(path.package("getgrib"),"data/ECMWF_t2m_demo.grib",sep="/")

   # Define the points you'll interpolate (nearest neighbor) for
   stations <- data.frame("name" = c("Innsbruck","Saentis"),
                          "lon"  = c(11.392778,9.343333),
                          "lat"  = c(47.267222,47.249167)  )

   # Getting neares neighbor from the grib file
   data <- getnearest(path,stations$lon,stations$lat)
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
