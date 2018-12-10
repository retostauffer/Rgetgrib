


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

