\name{interpolate}
\alias{interpolate}
\title{Grib-Files Interpolation for Multiple Stations (ecCodes based)}
\description{
Performing interpolation for a set of stations on a (set of) grib file(s).
Interpolation is coded in C using the ECMWF ecCodes library.

Please note that this function is in an alpha state and currently designed
to be able to rapidly interpolate ECMWF ensemble data which are on a regular
longitude latitdue grid.
}
\usage{
   ## Basic usage
   interpolate(files, stations, method = "bilinear", reshape = FALSE, verbose = FALSE)
}
\arguments{
   \item{file}{\code{character} string. File name of the grib file to be used.}
   \item{stations}{A \code{SpatialPointsDataFrame} which has at least to
      include one variable \code{statnr}. Can be either one single station
      or multiple statoins.}
   \item{method}{string, one of \code{"bilinear"} or \code{"nearest"}. See
       details.}
   \item{reshape}{logical. If \code{FALSE} (default) a long
      format will be returned. If \code{reshape=TRUE} a list of \code{data.frames}
      will be returned, each of it in a wide format (see details).}
    \item{verbose}{logical or any value which can be converted to integer.
        \code{FALSE} is equivalent to \code{0}, \code{TRUE} to \code{1}.
        For different values different levels of verbosity will be used.
        Currently implemented are \code{0}, \code{1}, \code{2} (very verbose).}
}
\details{
Allows to perform bilinear interpolation or nearest neighbour interpolation
(extraction) on grib files. Requires the ECMWF ecCodes library to be installed.

If \code{method = "bilinear"} bilinear interpolation on the grid is performed.
For \code{"bilinear"} if one of the four neighbouring grid points (NE, NW, SE, SW)
cannot be found the station lies outside the defined field. In this case
a constant value of \code{-9999} is returned.
If \code{method = "nearest"} the nearest grid point will be returned. 
For \code{"nearest"}: if a station lies further away than
\code{sqrt(deltalon^2 + deltalat^2) * 1.05} the station is considered to lie
outside the defined grid! In this case a constant value of \code{-9999} is
returned.

If \code{reshape = FALSE} a \code{data.frame} will be returned where each row
corresponds to one grib message. For each station one column will be added.  If
\code{reshape = TRUE} a \code{list} of \code{data.frame}s will be returned.
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

   x1 <- interpolate(file, stations, reshape = FALSE)
   print(head(x1))
   x2 <- interpolate(file, stations, reshape = TRUE)
   print(head(x2[[1]]))
   print(names(x2))

   n1 <- interpolate(file, stations, method = "nearest", reshape = FALSE)
   print(head(n1))
   n2 <- interpolate(file, stations, method = "nearest", reshape = TRUE)
   print(head(n2[[1]]))
   print(names(n2))

   # Testing different file types which are included in the
   # package. ECMWF data are anonymized (dates randomly changed
   # to when I've updated this package the last time).
   dir   <- paste(path.package("getgrib"),"data",sep="/")
   files <- list.files(dir,"^.*(grib|grib2)")
   for ( file in files ) {
      cat(sprintf(" * Testing file \"\%s\"",file))
      x1 <- interpolate(sprintf("\%s/\%s",dir,file),stations)
      print(head(x1))
      x2 <- interpolate(sprintf("\%s/\%s",dir,file),stations,reshape=TRUE)
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
