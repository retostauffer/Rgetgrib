\name{bilinear}
\alias{bilinear}
\title{Bilinear Interpolation on Grib-Files for Multiple Stations}
\description{
Performing bilinear interpolation for a set of stations on the grib
files directly. Rather than loading full fields from the grib file
(as \code{\link{getdata}} does) the interpolation is performed within
the C-code using the GRIBAPI.

Please note that this function is in an alpha state and currently designed
to be able to rapidly interpolate ECMWF ensemble data which are on a regular
longitude latitdue grid.

TODO: not yet checking \code{missingValue} from the grib file. Not yet
checking whether a station lies outside the extent of the gribfile!
}
\usage{
   ## Basic usage
   bilinear( file, stations, reshape=FALSE, levels=FALSE )
}
\arguments{
   \item{file}{\code{character} string. File name of the grib file to be used.}
   \item{stations}{A \code{SpatialPointsDataFrame} which has at least to
      include one variable \code{statnr}. Can be either one single station
      or multiple statoins.}
   \item{reshape}{\code{logical}, default \code{FALSE}. If \code{FALSE} a long
      format will be returned. If \code{reshape=TRUE} a list of \code{data.frames}
      will be returned, each of it in a wide format (see details).}
   \item{levels}{\code{logical}, default \code{FALSE}. If \code{TRUE} levels from
      grib files will be returned in resulting \code{data.frame}.}
}
\details{
Performs bilinar interpolation using C function and the GRIBAPI.

If \code{reshape=FALSE} a \code{data.frame} will be returned where each
row corresponds to one grib message. For each station one column will be
added.

If \code{reshape=TRUE} a \code{list} of \code{data.frame}s will be returned.
Each list element is a \code{data.frame} in the wide format and contains the
interpolated data for one specific station (see list element name).  The rows
of the \code{data.frame}s correspond to a specific datetime and variable, the
columns itself contain the individual members if the source file is an ensemble
and the perturbationNumber is defined in the grib message head.

Some additional attributes are attached to the result object such as 
\code{shortName}, \code{level}, and \code{typeOfLevel} from the grib meta
information, and some information when and with which \code{\link{getdata}}
package version was used to perform the interpolation (see attributes
\code{created} (time when and \code{package_version}).

Sincd October 2017 \code{stations$statnr} can also be of type \code{character}
or \code{factor}. The column name of the return should be adjusted correspondingly.
}
\examples{
   # Find package installation path including the demo grib file
   file <- paste(path.package("getgrib"),"data/ECEPS_12.grib",sep="/")

   crs <- crs("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
   stations <- data.frame(statnr=c(111,222,333),lon=c(10.111,10.555,10.99),
                          lat=c(46.888,47.222,47.111))
   stations <- SpatialPointsDataFrame(subset(stations,select=c(lon,lat)),
               data=subset(stations,select=statnr),proj4string=crs)

   x1 <- bilinear(file,stations,reshape=FALSE)
   print(head(x1))
   x2 <- bilinear(file,stations,reshape=TRUE)
   print(head(x2[[1]]))
   print(names(x2))

   # Testing different file types which are included in the
   # package. ECMWF data are anonymized (dates randomly changed
   # to when I've updated this package the last time).
   dir   <- paste(path.package("getgrib"),"data",sep="/")
   files <- list.files(dir,"^.*(grib|grib2)")
   for ( file in files ) {
      cat(sprintf(" * Testing file \"\%s\"",file))
      x1 <- bilinear(sprintf("\%s/\%s",dir,file),stations)
      print(head(x1))
      x2 <- bilinear(sprintf("\%s/\%s",dir,file),stations,reshape=TRUE)
      print(head(x2[[1]]))
   }
}
\seealso{
   \code{\link{gribdata2raster}},
   \code{\link{is_regular_ll_grid}},
   \code{\link{get_grid_increments}}
}
%- maybe also 'usage' for other objects documented here.
\author{Reto Stauffer}
