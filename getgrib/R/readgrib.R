# -------------------------------------------------------------------
# - NAME:        readgrib.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2018-12-11
# -------------------------------------------------------------------
# - DESCRIPTION: Super early alpha!
# -------------------------------------------------------------------
# - EDITORIAL:   2018-12-11, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-11 12:42 on marvin
# -------------------------------------------------------------------



readgrib <- function(file, shortName, level, steps, check = TRUE) {

    if ( is.character(file) ) file <- c(file)
    stopifnot(inherits(check, "logical"))

    stopifnot(file.exists(file))

    unique_keys = as.character(c("numberOfDataPoints", "Ni", "Nj",
        "longitudeOfFirstGridPointInDegrees", "longitudeOfLastGridPointInDegrees",
        "latitudeOfFirstGridPointInDegrees", "latitudeOfLastGridPointInDegrees"))
    .Call("getgrib_loadgrib", file, unique_keys,
          as.character(shortName),
          as.integer(level),
          as.integer(steps),
          as.integer(check))

}

