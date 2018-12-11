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
   interpolate(files, stations)
   ## Advanced settings
   interpolate(files, stations, method = "bilinear", k = 4,
               p = 1.0, reshape = FALSE, corr.lat = TRUE, verbose = FALSE)
}
\arguments{
   \item{file}{\code{character} string. File name of the grib file to be used.}
   \item{stations}{A \code{SpatialPointsDataFrame} which has at least to
      include one variable \code{statnr}. Can be either one single station
      or multiple statoins.}
   \item{method}{string, one of \code{"bilinear"}, \code{"nearest"}, or
       \code{"idw"}. See 'Details' section for more information.}
   \item{k}{integer, number of grid points to be considered. Only used if
       \code{method = "idw"}.}
   \item{p}{positive numeric, power parameter for the calculation of the
       inverse distance weights. Only used if \code{method = "idw"}, see
       'Details' section for more information.}
   \item{corr.lat}{logical. Only used for \code{method = "idw"}.
       If \code{TRUE} (default) a latitude-correction
       will be applied when calculating the distances between the target
       location/station and the grid points of the grib file. See 'Details'.}
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
For \code{"bilinear"} if one of the four neighbouring grid points (north east,
north west, south east, south west) cannot be found the station lies outside
the defined field. In this case \code{NA} will be returned.
If \code{method = "nearest"} the nearest grid point will be returned. 
For \code{"nearest"}: if a station lies further away than
\code{sqrt(deltalon^2 + deltalat^2) * 0.55} the station is considered to lie
outside the defined grid! In this case \code{NA} will be returned.
For \code{"idw"} the two parameters \code{k} and \code{p} have to be set.
\code{k} defines the number of grid points to be considered during 
interpolation. \code{p} is a power parameter. The larger, the stronger the
influence of the grid points closer to the station.
Only grid points not further away as \code{sqrt(deltalon^2 + deltalat^2) * 2.00}
from the target location will be considered as valid neighbours.

Bilinear interpolation:
\itemize{
    \item Find closest four grid points north east, north west, south east,
        and south west.
    \item If one grid point cannot be found \code{NA} will be returned.
    \item If all four grid points have been found: do simple bilinear
        interpolation.
}

Inverse distance weighting (idw):
\itemize{
    \item Find closest \code{k} grid points. If \code{k = 1} this is
        simply nearest neighbour interpolation.
    \item If not \code{k} valid neighbours can be found within the
        maximum distance (see above) a missing value will be returned.
    \item Calculate distance to each of the \code{k} neighbours. If
        \code{corr.lat = TRUE} latitude correction will be applied.
    \item Calculate individual weights as \code{w_j = (1. / distance_j)^p},
        calculate final weights relative to the sum of the weights
        of all neighbours \code{w_j = w_j / sum(w)} to ensure
        that the sum over the final weights is equal to \code{1.0}.
    \item Return weighted mean of all \code{k} neighbours given their
        weights \code{w_j}.
}

Nearest neighbour:
\itemize{
    \item Return value of nearest grid point.
    \item If no valid grid point within the maximum distance can
        be found (see above) a missing value will be returned.
    \item Technically it is inverse distance weighting with only
        one neighbour.
}


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

   # Bilinear interpolation
   x1 <- interpolate(file, stations, reshape = FALSE)
   print(head(x1))
   x2 <- interpolate(file, stations, reshape = TRUE)
   print(head(x2[[1]]))
   print(names(x2))

   # Nearest neighbour interpolation
   n1 <- interpolate(file, stations, method = "nearest", reshape = FALSE)
   print(head(n1))
   n2 <- interpolate(file, stations, method = "nearest", reshape = TRUE)
   print(head(n2[[1]]))
   print(names(n2))

   # Inverse distance weighting (default k = 4, p = 1.0, corr.lat = TRUE)
   i1 <- interpolate(file, stations, method = "idw", reshape = FALSE)
   print(head(i1))
   i2 <- interpolate(file, stations, method = "idw", reshape = TRUE)
   print(head(i2[[1]]))
   print(names(i2))

   # Inverse distance weighting with k = 1 is identical to nearest
   # neighbour interpolation.
   i3 <- interpolate(file, stations, method = "idw", reshape = FALSE, k = 1)
   idx <- grep("^station_.*$", names(n1))
   paste("Difference between nearest and idw(k=1): ", sum(n1[,idx] - i3[,idx]))

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
