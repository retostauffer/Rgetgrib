


readgrib <- function(file, steps, check = TRUE) {

    if ( is.character(file) ) file <- c(file)
    stopifnot(inherits(check, "logical"))

    stopifnot(file.exists(file))

    # If not set: reading all steps from grib file
    if ( missing(steps) ) steps <- -999.
    stopifnot(is.numeric(steps))

    unique_keys = as.character(c("numberOfDataPoints", "Ni", "Nj",
        "longitudeOfFirstGridPointInDegrees", "longitudeOfLastGridPointInDegrees",
        "latitudeOfFirstGridPointInDegrees", "latitudeOfLastGridPointInDegrees"))
    .Call("getgrib_readgrib", file, unique_keys, as.numeric(steps),
          as.integer(check))

}

