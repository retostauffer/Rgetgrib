\name{deaccumulate}
\alias{deaccumulate}
\alias{deaccumulate.gribdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Deaccumulates on Base of \code{gribdata} Objects}
\description{
   For some variables it is necessary to deaccumulate (e.g., precipitation
   in ECMWF). Data read by \code{\link{getdata}} (\code{gribdata} objects)
   can easily be deaccumulated using this function.

   Deaccumulation is simply the field \code{(t)} minus the field \code{(t-1)}.
}
\usage{
   ## Basic usage
   deaccumulate( x )
   ## With extra args: deaccumulate on 12h intervals
   deaccumulate( x, deaccumulation = 12 )
   ## Setting values below a threshold 'zeroval' to zero.
   deaccumulate( x, setzero = TRUE, zeroval = 0.001 )
}
\arguments{
   \item{x}{Object of type \code{gribdata}, see \code{\link{getdata}}.}
   \item{deaccumulation}{Numeric, deaccumulation period in hours. Default is \code{24}.}
   \item{zetzero}{Boolean, default \code{FALSE}. If set to \code{TRUE} all values below
      threshold \code{zeroval} will automatically be set to \code{0.}.}
}
\details{
   Uses the \code{gribdata} object and deaccumulates the data. 
   As unique identifier the meta information initdate, inittime, and
   member will be used. 

   \code{deaccumulation=24} means that the data will be deaccumulated 
   with \code{24}h time period.
}
\examples{
   # Find package installation path including the demo grib file
   file <- paste(path.package("getgrib"),"data/SnowSafeHindcast_201610130000.grib",sep="/")

   # Loading file, creates gribdata object. As this is
   # total precipitation I already scale them from meters to
   # millimeters using the 'scale="*1000"' argument.
   gribdata <- getdata(file,'tp',scale='* 1000')

   # Now we can to two things. Watching member 4 accumulated
   # fields:
   g1 <- gribdata2raster(gribdata)
   plot( g1[[which(grepl("_m04",names(g1)))]] )

   # OR we can deaccumulate the data first, and watch the
   # deaccumulated data. Please note that messages which
   # cannot be deaccumulated will automatically be removed!
   # Furthermore, 'setzero=TRUE' and 'zeroval=0.01' sets all
   # deaccumulated values below 0.01 (mm) to 0.00 (mm).
   gribdata_deaccumulated <- deaccumulate(gribdata,deaccumulation=24,setzero=TRUE,zeroval=0.01)
   g2 <- gribdata2raster(gribdata_deaccumulated)
   plot( g2[[which(grepl("_m04",names(g2)))]] )


}
\seealso{
   \code{\link{getdata}},
   \code{\link{deaccumulate}},
   \code{\link{gribdata2raster}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
