# -------------------------------------------------------------------
# - NAME:        manipulate_shortnames.R
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-04-22
# -------------------------------------------------------------------
# - DESCRIPTION: Used to create nice short names for the different
#                fields of the interpolated grib file(s).
#                Uses shortname, typeOfLevel, and level plus a set
#                of rules to rename the grib file short names.
#                Method is used in "bilinear", "nearestneighbour",
#                and other methods #TODO: others? inverse distance?
# -------------------------------------------------------------------
# - EDITORIAL:   2017-04-22, RS: Created file on thinkreto.
#                2017-04-23, RS: NA handling for stations outside grid
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2018-12-10 20:38 on marvin
# -------------------------------------------------------------------

# Manipulate variable names
manipulate_shortnames <- function( shortName, level, typeOfLevel ) {

   # Manipulte t2 and d2
   idx <- which( grepl("^2t$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "t2m"
   idx <- which( grepl("^2d$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "d2m"
   # Same with 100u and 100v
   idx <- which( grepl("^10fg$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "fg10"
   idx <- which( grepl("^10u$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "u10"
   idx <- which( grepl("^10v$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "v10"
   idx <- which( grepl("^100u$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "u100"
   idx <- which( grepl("^100v$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "v100"

   # For variables on isobaricInHectopascal add level 
   idx <- which( grepl("^isobaricInhPa$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s%d",shortName[idx],level[idx])

   # For hybrid levels (model level data)
   idx <- which( grepl("^hybrid$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s_%d",shortName[idx],level[idx])

   return( matrix(shortName,ncol=1,dimnames=list(NULL,'shortName')) )

}
















