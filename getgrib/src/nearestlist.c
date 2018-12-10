
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <eccodes.h>

////* Search for closest north east grid point */
///int closest_northeast(double stnlon, double stnlat, int n,
///                      double *lons, double *lats, double *dist, int verbose) {
///   int res = -9;
///   int i;
///   for ( i = 0; i < n; i++ ) {
///      /* Gridpoint not onrtheast from station location */
///      if ( lons[i] <= stnlon || lats[i] <= stnlat ) { continue; }
///      if ( res < 0 ) { res = i; continue; }
///      if ( dist[i] < dist[res] ) { res = i; }
///   }
///   if ( verbose >= 2 ) { Rprintf("    Closest NE:  %10.5f %10.5f  (gp %d)\n",lons[res],lats[res],res); }
///   return res;
///}
////* Search for closest south east grid point */
///int closest_southeast(double stnlon, double stnlat, int n,
///                      double *lons, double *lats, double *dist, int verbose) {
///   int res = -9;
///   int i;
///
///   for ( i = 0; i < n; i++ ) {
///      if ( lons[i] <= stnlon || lats[i] > stnlat ) continue;
///      if ( res < 0 ) { res = i; continue; }
///      if ( dist[i] < dist[res] ) { res = i; }
///   }
///   if ( verbose >= 2 ) { Rprintf("    Closest SE:  %10.5f %10.5f  (gp %d)\n",lons[res],lats[res],res); }
///   return res;
///}
////* Search for closest south west grid point */
///int closest_southwest(double stnlon, double stnlat, int n,
///                      double *lons, double *lats, double *dist, int verbose) {
///   int res = -9;
///   int i;
///
///   for ( i = 0; i < n; i++ ) {
///      if ( lons[i] > stnlon || lats[i] > stnlat ) continue;
///      if ( res < 0 ) { res = i; continue; }
///      if ( dist[i] < dist[res] ) { res = i; }
///   }
///   if ( verbose >= 2 ) { Rprintf("    Closest SW:  %10.5f %10.5f  (gp %d)\n",lons[res],lats[res],res); }
///   return res;
///}
////* Search for closest north west grid point */
///int closest_northwest(double stnlon, double stnlat, int n,
///                      double *lons, double *lats, double *dist, int verbose) {
///   int res = -9;
///   int i;
///
///   for ( i = 0; i < n; i++ ) {
///      //Rprintf(" ------------- %10.5f %10.5f\n",lats[i],stnlat);
///      if ( lons[i] > stnlon || lats[i] <= stnlat ) continue;
///      if ( res < 0 ) { res = i; continue; }
///      if ( dist[i] < dist[res] ) { res = i; }
///   }
///   if ( verbose >= 2 ) { Rprintf("    Closest NW:  %10.5f %10.5f  (gp %d)\n",lons[res],lats[res],res); }
///   return res;
///}
////* Computes euclidean distance */
///double compute_distance(double x1,double x2,double y1,double y2) {
///   double res;
///   res = sqrt(pow(x1-x2,2.) + pow(y1-y2,2.));
///   return res;
///}
////* Given station location stnlon and stnlat this function searches for the
///   4 nearest grid points in quadrants. Returns an integer vector with 4
///   values containing indizes for the grid points (NE,SE,SW,NW) */
///int * get_surrounding_grid_point_indizes(int statnr, double stnlon,double stnlat,int n,
///         double * lons,double * lats,int verbose) {
///
///   int i;
///   static int res[4];
///   double dist[n];
///
///   /* Compute distance to each grid point */
///   for ( i=0; i<n; i++ ) { dist[i] = compute_distance(stnlon,lons[i],stnlat,lats[i]); }
///
///   /* Searching for nearest neighbor gridpoints, quadrants, regular_ll! */
///   if ( verbose >= 2 ) {
///      Rprintf("    Station:     %d\n",statnr);
///      Rprintf("                 %10.5f %10.5f\n",stnlon,stnlat);
///   }
///   res[0] = closest_northeast(stnlon,stnlat,n,lons,lats,dist,verbose);
///   res[1] = closest_southeast(stnlon,stnlat,n,lons,lats,dist,verbose);
///   res[2] = closest_southwest(stnlon,stnlat,n,lons,lats,dist,verbose);
///   res[3] = closest_northwest(stnlon,stnlat,n,lons,lats,dist,verbose);
///
///   return res;
///}
////* Compute weights for interpolation. Scematic plot.
///   S: station of interest.
///   0/1/2/3: neighboring points
///   w: the weights (zonal/meridional)
///
///   (NE) 3 + ----------+------ + 0 (NE)      \
///          |           |       |             |
///          |                   | w[0]        |
///          |           |       |             |
///          + - - - - - S - - - +             |- deltalat
///          |           |       |             |
///          |                   | w[1]        |
///          |           |       |             |
///   (SE) 2 + ----------+------ + 1 (SE)      /
///              w[3]      w[2]
///
///          \___________________/
///                 deltalon
///
///*/
///double * get_interpolation_weights(double stnlon, double stnlat,
///         double * lons, double * lats, double deltalon, double deltalat, int * neighbors) {
///   static double res[4];
///   int i;
///
///   res[0] = 1. - (lats[neighbors[0]] - stnlat) / deltalat;
///   res[1] = 1. - (stnlat - lats[neighbors[1]]) / deltalat;
///   res[2] = 1. - (lons[neighbors[1]] - stnlon) / deltalon;
///   res[3] = 1. - (stnlon - lons[neighbors[2]]) / deltalat;
///   //for ( i = 0; i < 4; i++ ) {
///   //   Rprintf("    Weight %d:   %10.5f\n",i,res[i]);
///   //}
///   return res;
///}
///
////* Interpolate */
///double get_interpolated_value(int * neighbors, double * weights, double * values,
///         double deltalon, double deltalat) {
///
///   static double res;
///   res = weights[2] * ( weights[0] * values[neighbors[0]] + weights[1] * values[neighbors[1]] ) +
///         weights[3] * ( weights[0] * values[neighbors[3]] + weights[1] * values[neighbors[2]] );
///   return res; 
///}
///
///int intArrayMin(int * x, int n) {
///   int i, min = x[0];
///   for ( i=1; i<n; i++ ) { if ( x[i] < min ) { min = x[i]; } }
///   return( min );
///}
/* Search for closest north east grid point */
int get_nearest_grid_point_index(double stnlon, double stnlat, int n,
                      double *lons, double *lats, double maxdist, int verbose) {
    int res = -9;
    double dist, tmp;
    int i;
    for ( i = 0; i < n; i++ ) {
       tmp  = sqrt(pow(stnlon - lons[i], 2.) + pow(stnlat - lats[i], 2.));
       if ( tmp > maxdist ) { continue; }
       /* Gridpoint not onrtheast from station location */
       if ( res < 0 || tmp < dist ) {
           dist = tmp; res  = i;
       }
    }
    if ( verbose >= 2 ) {
       Rprintf("    Closest GP to %10.5f %10.5f is %10.5f %10.5f  (gp %d)\n",
               stnlon, stnlat, lons[res], lats[res], res);
    }
    return res;
}


/* -------------------- MAIN FUNCTION ------------------- */
SEXP grib_nearest_interpolation(SEXP gribfile, SEXP statnr, SEXP statlon, SEXP statlat,
      SEXP verbose)
{

    int i, nprotected=0, err=0;
    int nstat = length(statnr);
 
    double *statnrptr  = REAL(statnr);
    double *statlonptr = REAL(statlon);
    double *statlatptr = REAL(statlat);
    int    *verboseptr = INTEGER(verbose);
 
    /* Variables to store data from grib messages */
    int    nmsg = 0, msgcount = 0;
    size_t values_len = 0;
    double *values = NULL, *lons = NULL, *lats = NULL;
    double missingvalue = -9999.0;
    int    minint;
    int    nearest;
    double deltalon, deltalat, maxdist;
    long   level, olevel;
    char   typeOfLevel[50];
    char   shortName[50];
    size_t str50 = 50;
    int    allocsize = -9;
 
    /* Open grib file handler */
    const char *file;
    file = CHAR(STRING_ELT(gribfile,0));
    FILE* in = NULL;
    grib_handle *h = NULL; /* grib handling pointer */
 
    /* ----------------------- end of header section -------------------- */
 
    /* Open grib file */
    in = fopen(file,"r");
    if ( ! in ) {
        Rprintf("[C] Cannot open grib file: %s\n",file);
        SEXP rval = PROTECT(allocVector(REALSXP,1)); ++nprotected;
        double *rvalptr = REAL(rval); rvalptr[0] = -99999.;
        UNPROTECT(nprotected);
        return rval;
    }
    if ( verboseptr[0] >= 2 ) { Rprintf("[C] Processing file %s\n",file); }
 
    
    /* Loop over stations, station list */
    if ( verboseptr[0] >= 2 ) {
        Rprintf(" * Station list output\n");
        for ( i = 0; i < nstat; i++ ) {
            Rprintf("   %5.0f  %10.5f %10.5f\n", statnrptr[i], statlonptr[i], statlatptr[i]);
        }
    }
 
    /* Number of messages */
    while ( (h = grib_handle_new_from_file(0,in,&err)) != NULL ) {
        nmsg++;
        grib_handle_delete(h);
    }
    if ( verboseptr[0] >= 1 ) { Rprintf(" * Number of messages to read: %d\n",nmsg); }
    rewind(in);
 
 
    /* Create output variable rval for interpolated values */
    SEXP rval = PROTECT(allocMatrix(REALSXP,nmsg,nstat)); ++nprotected;
    double *rvalptr = REAL(rval);
    for ( i = 0; i < (nstat*nmsg); i++ ) { rvalptr[i] = -99999; } /* Setting defaults */
 
    /* Create output matrix for meta information */
    SEXP meta = PROTECT(allocMatrix(REALSXP,nmsg,4)); ++nprotected;
    double *metaptr = REAL(meta);
    for ( i = 0; i < (nmsg*4); i++ ) { metaptr[i] = -99999; } /* Setting defaults */
 
    /* Store shortName, level, and typeOfLevel meta information */
    SEXP meta_name  = PROTECT(Rf_allocVector(STRSXP, nmsg));  ++nprotected;
    SEXP meta_ltype = PROTECT(Rf_allocVector(STRSXP, nmsg));  ++nprotected;
    SEXP meta_level = PROTECT(allocVector(REALSXP, nmsg));    ++nprotected;
    double *meta_levelptr = REAL(meta_level);
 
    /* Open grib file connection */
    ///h = grib_handle_new_from_file(0,in,&err);
    msgcount = 0;
    while ( (h = grib_handle_new_from_file(0,in,&err)) != NULL ) {
 
        /* Getting size of the message */
        GRIB_CHECK(grib_get_size(h,"values",&values_len),0);
        if ( verboseptr[0] >= 2 ) { Rprintf(" * Grib message size: %d\n",values_len); }
 
        /* Allocate vectors to load/store data */
        if ( allocsize < 0 ) {
            if ( verboseptr[0] >= 1 ) { Rprintf(" * Allocte arrays with %d\n",values_len); }
            values = malloc(values_len*sizeof(double)); /* values    */
            lons   = malloc(values_len*sizeof(double)); /* longitude */
            lats   = malloc(values_len*sizeof(double)); /* latitude  */
            allocsize = values_len;
        } else if ( ! allocsize == values_len ) {
            if ( verboseptr[0] >= 1 ) { Rprintf(" * Re-allocte arrays with %d\n",values_len); }
            values = realloc(values,values_len*sizeof(double)); /* values    */
            lons   = realloc(lons,values_len*sizeof(double)); /* longitude */
            lats   = realloc(lats,values_len*sizeof(double)); /* latitude  */
            allocsize = values_len;
        }
 
        /* Getting data values */
        GRIB_CHECK(grib_get_double_array(h, "values",     values, &values_len), 0);
        GRIB_CHECK(grib_get_double_array(h, "longitudes", lons,   &values_len), 0);
        GRIB_CHECK(grib_get_double_array(h, "latitudes",  lats,   &values_len), 0);
 
        /* Getting grid increments */
        GRIB_CHECK(grib_get_double(h, "iDirectionIncrementInDegrees", &deltalon),0);
        GRIB_CHECK(grib_get_double(h, "jDirectionIncrementInDegrees", &deltalat),0);
        if ( verboseptr[0] >= 2 ) {
           Rprintf(" * Grid increments (lon): %10.5f    (lat): %10.5f\n", deltalon, deltalat);
        }
        /* Maximum distance allowed before expecting that a station
         * is outside the defined grid */
        maxdist = sqrt(deltalon * deltalon + deltalat * deltalat) * 1.05;
        if ( verboseptr[0] >= 2 ) {
            Rprintf(" * Maximum distance to considered \"in grid\": %.5f degrees\n", maxdist);
        }
 
        /* Meta information */
        GRIB_CHECK(grib_get_double(h, "dataDate",&metaptr[msgcount+0*nmsg]),0);
        GRIB_CHECK(grib_get_double(h, "dataTime",&metaptr[msgcount+1*nmsg]),0);
        GRIB_CHECK(grib_get_double(h, "step",    &metaptr[msgcount+2*nmsg]),0);
 
        /* Check perturbationNumber. Load if key exists, else set to 0 */
        int check = grib_get_double(h, "perturbationNumber",&metaptr[msgcount+3*nmsg]);
        if ( check == 0 ) {
            GRIB_CHECK(grib_get_double(h, "perturbationNumber",&metaptr[msgcount+3*nmsg]),0);
        } else { metaptr[msgcount+3*nmsg] = 0; }
 
        /* Load shortname, typeOfLevel, and level */
        grib_get_long(h, "level", &level);
        meta_levelptr[msgcount] = level;
        str50 = 50; grib_get_string(h, "typeOfLevel", typeOfLevel,&str50);
        str50 = 50; grib_get_string(h, "shortName",   shortName,&str50);
        SET_STRING_ELT(meta_name,  msgcount, Rf_mkChar(shortName));
        SET_STRING_ELT(meta_ltype, msgcount, Rf_mkChar(typeOfLevel));
        //Rprintf(" ----------------------- %s %s %d\n",shortName,typeOfLevel,level);
 
 
        /* Interpolate station by station */
        for ( i = 0; i < nstat; i++ ) {
            /* Find closest grid point */
            nearest = get_nearest_grid_point_index(statlonptr[i],statlatptr[i],
                           values_len, lons, lats, maxdist, verboseptr[0]);
            /* Extract value */
            if ( nearest < 0 ) {
                rvalptr[ msgcount + nmsg*i ] = missingvalue;
            } else {
                rvalptr[ msgcount + nmsg*i ] = values[nearest];
            }
        }
 
        /* Increase message counter */
        msgcount++;
        grib_handle_delete(h);
    }
 
    /* Release grib file */ 
    fclose( in );
    in = NULL;
 
    /* Construct result list from variables containing the results */
    SEXP res   = PROTECT(allocVector(VECSXP,    5));  ++nprotected;
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 5));  ++nprotected;
    SET_STRING_ELT(names, 0, Rf_mkChar("data"));
    SET_STRING_ELT(names, 1, Rf_mkChar("meta"));
    SET_STRING_ELT(names, 2, Rf_mkChar("shortName"));
    SET_STRING_ELT(names, 3, Rf_mkChar("level"));
    SET_STRING_ELT(names, 4, Rf_mkChar("typeOfLevel"));
    Rf_setAttrib(res, R_NamesSymbol, names);
 
    SET_VECTOR_ELT(res, 0, rval);
    SET_VECTOR_ELT(res, 1, meta);
    SET_VECTOR_ELT(res, 2, meta_name);
    SET_VECTOR_ELT(res, 3, meta_level);
    SET_VECTOR_ELT(res, 4, meta_ltype);

    /* Release allocated memory and unprotect variables */ 
    free(values);    free(lons); free(lats);
    UNPROTECT(nprotected);

    /* Return results vector */
    return res;
}

