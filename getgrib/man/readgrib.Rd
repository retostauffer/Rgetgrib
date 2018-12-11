\name{readgrib}
\alias{readgrib}
\title{Reading Full Grib Files from Grib Files}
\description{Allows to retrieve complete fields from a grib1/grib2 file
    based on the ECMWF ecCodes library with additional meta information.

    Please note that this method is in a very early alpha state and neither
    tested not fail safe!
}
\usage{
    readgrib(file, shortName, level, steps, check = TRUE)
}
\arguments{
    \item{file}{character, path to the grib file.}
    \item{shortName}{character, \code{shortName} as in the grib meta info.}
    \item{level}{integer, \code{level} as in the grib meta info.}
    \item{steps}{integer or list of integers to specify forecast steps
        to be extracted.}
    \item{check}{logical, default is \code{TRUE}. Checks something.}
}
\details{
    Returns:
    \itemize{
        \item \code{steps} forecast steps
        \item \code{dates} forecast dates
        \item \code{times} forecast times
        \item \code{lons} longitudes of the grid
        \item \code{lats} latitudes of the grid
        \item \code{data} array containing the data
        \item \code{shortName} grib short name of the variable(s)
        \item \code{indicatorOfTypeOfLevel} level type of the variable(s)
        \item \code{level} level of the variable(s)
        \item \code{member} member (ensemble member) if needed. Else \code{0}.
    }
\value{
    Returns a list with the data and a set of meta information
    from the grib file meta data. See 'Details' section for more
    information about the return values.
}
\examples{
   dir   <- paste(path.package("getgrib"),"data",sep="/")
   file  <- paste0(dir, "/GFS_12.grib")

   data <- readgrib(file, "2t", 2, 12)
   image(data$data[,,1])

   # This yields a seg fault (TODO: super early alpha!
   #data <- readgrib(file, "2t", 0, 12) 
}

