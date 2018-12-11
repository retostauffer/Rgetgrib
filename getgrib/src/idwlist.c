
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <eccodes.h>

/* Searching for conditionally closest grid point (k = 1, ..., K)
 *
 * *closest: vector of integers (length K) taking up the grid point index
 *           or number of the K closest stations (if available).
 * *weights: vector of doubles (length K) taking up the interpolation weights.
 * valid:    integer, is set to 1 if all K neighbours have been found, else
 *           set to 0 (no valid neighbour information, interpolation cannot
 *           be performed).
 * stnlon:   longitude of target location
 * stnlat:   latitude of target location
 * n:        number of grid points in the grib file
 * K:        number of grid points to look for. For nearest neighbour
 *           interpolation this is simply 1 (the one closest to the
 *           target location). In case of inverse distance weighting
 *           this number is > 1.
 * P:        numeric, power parameter for the weights of the inverse
 *           distance weighting. No effect if K = 1.
 * corrlat:  integer. If 1 latitude correction is done, else not.
 * *lons:    longitudes of grid points in grib file (length n)
 * *lats:    latitudes of grid points in grib file (length n)
 * maxdist:  maximum distance between the target location and a grid
 *           point. If the distance between these two points exceed
 *           "maxdist" the grid point is considered to not be close
 *           to the target location (used to identify target locations
 *           outside the defined grid).
 * verbose:  an integer used to produce verbose output on stdout.
 */
void get_k_nearest_grid_points_and_weights(int *closest, double *weights, int *valid,
                      double stnlon, double stnlat, int n, int K, double P, int corrlat,
                      double *lons, double *lats, double maxdist, int verbose) {

    double tmp;
    double corr;
    double dist[K];
    const double pi = 3.141593;
    int i, j, k;
    int dupl = 0;
    double hittolerance = 1e-8; /* tolerance (distance) for a station to be considered
                                 * to lie on a grid point */
    /* Initial guess: we will find valid K neighbours for the target
     * location. Set valid = 1. */
    *valid = 1;

    /* Initial values for "closest": set all to -9, all weights to 1.0 */
    for ( k = 0; k < K; k++ ) { dist[k] = -999.; closest[k] = -9; weights[k] = 0.0; }

    /* Loop to find K closest grid points */
    for ( k = 0; k < K; k++ ) {
        for ( i = 0; i < n; i++ ) {
            dupl = 0;
            /* Calculate distance with or without latitude correction */
            if ( corrlat == 0 ) {
                tmp  = sqrt(pow(stnlon - lons[i], 2.) + pow(stnlat - lats[i], 2.));
            } else {
                corr = cos((stnlat + lats[i]) / 360. * pi);
                tmp  = sqrt(pow(stnlon - lons[i], 2.) + pow(corr * (stnlat - lats[i]), 2.));
            }

            /* Too far away? Skip */
            if ( tmp > maxdist ) { continue; }

            /* First grid point (closest to station): consider
             * the current grid point as closest if the distance
             * "tmp < dist[k]", the iteration-distance.
             * For k > 0: check if we have already selected this
             * grid point. If so, ignore, else check if distance tmp
             * is smaller than dist[k] and take if condition is true. */
            if ( k > 0 ) { 
                for ( j = 0; j < k; j++ ) { 
                    if ( closest[j] == i ) { dupl = 1; break; }
                }
            }
            /* Update dist[k] and closest[k] if we have found a
             * grid point closer to the target location than the
             * one in the last iteration over i = 0, ..., n */
            if ( dupl == 0 && (closest[k] < 0 || tmp < dist[k]) ) {
                dist[k] = tmp; closest[k] = i;
            }
        }

        /* No grid point found fulfilling the condition that 
         * the distance between the grid point and the target location
         * is <= maxdist? Return here (will result in closest[...] to 
         * contain negative values, used to capture grid points outside
         * the grid). In this case valid will be set to 0 and returned
         * to the main function. */
        if ( closest[k] < 0 ) { *valid = 0; k = K; } /* k = K stops loop */

        /* Perfect match? Leave closest[k > 0] == -9, used to identify
         * perfect hits */
        if ( k == 0 && dist[k] < hittolerance ) {
            weights[k] = 1.0; dist[k] = 0.0; k = K; /* k = K stops loop */
        }

    }

    /* Calculate weights */
    double weightssum = 0.;
    if ( closest[0] > 0 && dist[0] > hittolerance ) {
        for ( k = 0; k < K; k++ ) { weights[k] = pow(1. / dist[k], P); }
        for ( k = 0; k < K; k++ ) { weightssum += weights[k]; }
        for ( k = 0; k < K; k++ ) { weights[k] = weights[k] / weightssum; }
    }

    /* ----------------------------------------------- */
    /* Verbose output */
    if ( verbose > 0 ) {
        Rprintf("\nSummary of closest grid points for ");
        Rprintf("target location %.5f %.5f\n", stnlon, stnlat);

        /* No valid neighbours found? */
        if ( *valid == 0 ) {
            Rprintf(" - No valid K = %d neighbours found.\n\n", K);
            return;
        }
        /* Perfect hit? */
        if ( K > 1 ) {
            if ( closest[0] >= 0 && closest[1] < 0 ) {
                Rprintf(" - Target location lies exactly on a grid point\n");
                K = 1;
            }
        }
        for ( k = 0; k < K; k++ ) {
            Rprintf(" - %d: %7.4f %7.4f (%4d), distance %6.4f, weight = %5.4f\n",
                    k + 1, lons[closest[k]], lats[closest[k]], closest[k], dist[k], weights[k]);
        }
    }
    /* ----------------------------------------------- */

}


/* -------------------- MAIN FUNCTION ------------------- */
SEXP grib_idw_interpolation(SEXP gribfile, SEXP statnr, SEXP statlon, SEXP statlat,
      SEXP K, SEXP P, SEXP corrlat, SEXP verbose)
{

    int i, k, nprotected=0, err=0;
    int nstat = length(statnr);
 
    double *statnrptr  = REAL(statnr);     /* Station number/identifier */
    double *statlonptr = REAL(statlon);    /* Target location longitude */
    double *statlatptr = REAL(statlat);    /* Target location latitude */
    double *Pptr       = REAL(P);          /* Power parameter for IDW */
    int    *Kptr       = INTEGER(K);       /* Number of neighbouring grid points */
    int    *corrlatptr = INTEGER(corrlat); /* Flag for latitude correction */
    int    *verboseptr = INTEGER(verbose); /* Verbosity level */

    /* The following values are returned by the
     * get_k_nearest_grid_points_and_weights function */
    int    valid;                          /* Flag used to identify points outside grid */
    int    closest[Kptr[0]];               /* Vector of closest grid points */
    double weights[Kptr[0]];               /* Weights for neighbouring grid points */
 
    /* Variables to store data from grib messages */
    int    nmsg = 0, msgcount = 0;
    size_t values_len = 0;
    double *values = NULL, *lons = NULL, *lats = NULL;
    double missingvalue = -9999.0;
    int    minint;
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
        if ( verboseptr[0] >= 2 ) {
            Rprintf("====================== (MESSAGE %d)\n", msgcount);
            Rprintf("Grib message %d, size: %d\n", msgcount, values_len);
        }
 
        /* Allocate vectors to load/store data */
        if ( allocsize < 0 ) {
            if ( verboseptr[0] >= 1 ) { Rprintf("Allocte arrays with %d\n",values_len); }
            values = malloc(values_len*sizeof(double)); /* values    */
            lons   = malloc(values_len*sizeof(double)); /* longitude */
            lats   = malloc(values_len*sizeof(double)); /* latitude  */
            allocsize = values_len;
        } else if ( ! allocsize == values_len ) {
            if ( verboseptr[0] >= 1 ) { Rprintf("Re-allocte arrays with %d\n",values_len); }
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
           Rprintf("Grid increments (lon): %10.5f    (lat): %10.5f\n", deltalon, deltalat);
        }
        /* Maximum distance allowed before expecting that a station
         * is outside the defined grid */
        if ( Kptr[0] == 1 ) {
            maxdist = sqrt(deltalon * deltalon + deltalat * deltalat) * 1.00;
        } else {
            maxdist = sqrt(deltalon * deltalon + deltalat * deltalat) * 4.00;
        }
        if ( verboseptr[0] >= 2 ) {
            Rprintf("Maximum distance to considered \"in grid\": %.5f degrees\n", maxdist);
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
            get_k_nearest_grid_points_and_weights(closest, weights, &valid,
                           statlonptr[i],statlatptr[i],
                           values_len, Kptr[0], Pptr[0], corrlatptr[0],
                           lons, lats, maxdist, verboseptr[0]);

            /* Not able to find Kptr[0] valid neighbours */
            if ( valid == 0 ) {
                rvalptr[msgcount + nmsg*i] = missingvalue;
            } else {
                /* idw with only one grid point is simply nearest nieghbour.
                 * In this case use closest[0] as return value. */
                if ( Kptr[0] == 1 ) {
                    rvalptr[msgcount + nmsg*i] = values[closest[0]];
                } else {
                    /* Perfect hit (station lies on a grid point) */
                    if ( closest[0] >= 0 && closest[1] < 0 ) {
                        rvalptr[msgcount + nmsg*i] = values[closest[0]];
                    } else {
                        rvalptr[msgcount + nmsg*i] = 0.;
                        for ( k = 0; k < Kptr[0]; k++ ) {
                            rvalptr[msgcount + nmsg*i] += values[closest[k]] * weights[k]; 
                        }
                    }
                }
            }
        } /* End of loop over stations (i) */
 
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

