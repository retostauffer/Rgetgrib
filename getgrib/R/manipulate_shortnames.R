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
# - L@ST MODIFIED: 2020-02-05 10:25 on marvin
# -------------------------------------------------------------------

# Manipulate variable names
manipulate_shortnames <- function( shortName, level, typeOfLevel ) {

   # Manipulte t2 and d2
   idx <- which( grepl("^2t$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "t2m"
   idx <- which( grepl("^2d$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "d2m"
   idx <- which( grepl("^10fg$",shortName) )
   # Renaming gusts: 10fg -> fg10m
   # Aggregated gusts: 10fg3 -> fg10m_3
   if ( length(idx) > 0 ) shortName[idx] <- "fg10m"
   idx <- which( grepl("^10fg",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- gsub("^10fg", "fg10m_", shortName[idx])
   # Same with 100u and 100v
   idx <- which( grepl("^10u$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "u10m"
   idx <- which( grepl("^10v$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "v10m"
   idx <- which( grepl("^100u$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "u100m"
   idx <- which( grepl("^100v$",shortName) )
   if ( length(idx) > 0 ) shortName[idx] <- "v100m"

   # For variables on tropopause niveau
   idx <- which( grepl("^tropopause$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s_tropo",shortName[idx],level[idx]/100)

   # For variables on pressureFromGroundLayer
   idx <- which( grepl("^pressureFromGroundLayer$",typeOfLevel) )
   if ( length(idx) > 0 )
      shortName[idx] <- sprintf("%s%dhPa_agl",shortName[idx],level[idx]/100)

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
















