#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "eccodes.h"


/* (1) Helper functions. */
SEXP getListElement(SEXP list, const char *str)
{
  SEXP elmt, names;
  PROTECT(elmt  = R_NilValue);
  PROTECT(names = getAttrib(list, R_NamesSymbol));

  for(int i = 0; i < length(list); i++) {
    if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
      elmt = VECTOR_ELT(list, i);
      break;
    }
  }
  UNPROTECT(2);
  return elmt;
}

/* (1) Helper functions. */
int getListInt(SEXP list, const char *str)
{
  SEXP res = getListElement(list, str);
  return INTEGER(res)[0];
}

/* (1) Helper functions. */
double getListReal(SEXP list, const char *str)
{
  SEXP res = getListElement(list, str);
  return REAL(res)[0];
}


/* Combine two strings s1 and s2 with separator 'sep'.
 * 'sep' can also be an empty string ("").
 * @param sep character string, separator.
 * @param s1,s2 character strings.
 * @returns String, combined "<s1><sep><s2>". */
char *join(const char* sep, const char* s1, const char* s2)
{
    char* result = malloc(strlen(s1) + strlen(s2) + strlen(sep) + 1);

    if (result) // thanks @pmg
    {
        if ( strlen(s1) == 0 ) {
            strcpy(result, s2);
        } else {
            strcpy(result, s1);
            if ( strlen(sep) > 0 ) { strcat(result, sep); }
            strcat(result, s2);
        }
    }

    return result;
}

/* If the user request includes a set of steps we have to check
 * whether or not these steps are available in the grib file.
 * This function returns 0 if the step is available in the gri
 * file or 9 if not.
 * @param stp the step to be checked (needle)
 * @param steps array of longs, available steps (haystack)
 * @param n length of array steps
 * @returns Returns 0 if "stp" is in "steps", 9 else.
 */
int _getgrib_check_step_in_array_(long stp, long *steps, size_t n) {
    int i;
    for ( i = 0; i < n; i++ ) {
        if ( stp == steps[i] ) { return 0; }
    }
    return 9;
}

/* Checking grib files and extracting available steps/required
 * steps.
 * @param file character string, name of the file to be read.
 * @param unique_keys character array with grib key names to be checked.
 *      If the keys specified here are not unique (e.g., different fields
 *      or variable names or resolution) the script will exit.
 * @param steps_req double, can be of length 1 "-999." or a set of 
 *      steps t obe read. If "-999." all available steps form the
 *      grib files will be returned.
 * @param check integer pointer, will be set to 0 if evertyhing is ok,
 *      something >0 if there problems occured and the check failed.
 * @param TODO ...
 */
void _getgrib_check_grib_fields_(const char *file, SEXP unique_keys, int *check)
{

    ///* Variables for ecCodes */
    ///codes_handle *msg = NULL;    /* Grib message handler */
    codes_index* grbidx = NULL;  /* Grib index */
    int err = 0;                 /* Error flag variable */
    int ret = 0;                 /* Index return value */
    int i;

    /* Create comma separated list for the grib index.
     * A combination of "step" and what the user defines to be
     * constant*/
    char *grbkeys = "";
    for ( i = 0; i < length(unique_keys); i++ ) {
        grbkeys = join(",", grbkeys, CHAR(STRING_ELT(unique_keys, i)));
    }
    ///Rprintf("\n\n%s\n\n", grbkeys);

    /* Create new index */
    grbidx = codes_index_new(0, grbkeys, &ret);

    /* Indexing the current file */
    ret  = codes_index_add_file(grbidx, file);
    if ( ret ) { Rprintf("error: %s\n",codes_get_error_message(ret)); exit(ret); }

    /* For now I am expecting to have only one set of parameters in the
     * grib file and that all grib messages do have the very same extent.
     * Checking this using indizes here.
     */
    size_t keylen;
    const char *key;
    for ( i = 0; i < length(unique_keys); i++ ) {
        key = CHAR(STRING_ELT(unique_keys, i));
        CODES_CHECK(codes_index_get_size(grbidx, key, &keylen), 0);
        if ( keylen != 1 ) {
            Rprintf("unique key \"%s\" (as specified) is not unique!\n", key);
            /* Release the index */
            codes_index_delete(grbidx);
            *check = 9; return; /* Nothing is ok! */
        }
    }


    // Setting "return value" (void)
    *check = 0; /* Return 0 (everything looks ok) */
}


/* Creates a small double-vector which is used to compare the
 * content of different grib messages.
 * @param msg codes_handle object, message handler.
 * @return Returns a vector of doubles with (currently) 6 elements
 *      containing Ni/Nj (dimension of the grid) and lonmin/lonmax/latmin/latmax
 *      which specify the corners of the grid to check whether or not the
 *      domain is the very same.
 */
double* _getgrib_get_grib_info_(codes_handle *msg)
{
    double* info;
    info = malloc(sizeof(double)*6);

    codes_get_double(msg, "Ni", &info[0]);
    codes_get_double(msg, "Nj", &info[1]);
    codes_get_double(msg, "longitudeOfFirstGridPointInDegrees", &info[2]);
    codes_get_double(msg, "longitudeOfLastGridPointInDegrees",  &info[3]);
    codes_get_double(msg, "latitudeOfFirstGridPointInDegrees",  &info[4]);
    codes_get_double(msg, "latitudeOfLastGridPointInDegrees",   &info[5]);

    return info;
}

/* Compares the extent of two grib messages.
 * @param msg codes_handle message handler of the message to be checked
 * @param file character, name of the file where msg comes from (only for
 *      output/debugging purposes).
 * @param *ref double pointer, a vector returned by _getgrib_get_grib_info_(...)
 *      containing the values to be compared against.
 * @return No return values (void function), exit's if the meta information
 *      of msg and the referenc differ.
 */
void _getgrib_check_grib_info_(codes_handle *msg, const char *file, double *ref)
{
    double *info;
    info = _getgrib_get_grib_info_(msg);

    for (int i = 0; i < 6; i++) {
        if ( info[i] != ref[i] ) {
            Rprintf("Grib info check error: %.5f != %.5f\nIn file \"%s\"\n",
                    info[i], ref[i], file);
            exit(9);
        }
    }

    info = NULL;
}

/* Reading grib data
 * @param file character string, name of the file to be read.
 * @param unique_keys character array with grib key names to be checked.
 *      If the keys specified here are not unique (e.g., different fields
 *      or variable names or resolution) the script will exit.
 * @param steps_req double, can be of length 1 "-999." or a set of 
 *      steps t obe read. If "-999." all available steps form the
 *      grib files will be returned.
 * @param 
 */
SEXP getgrib_loadgrib(SEXP gribfile, SEXP unique_keys, 
        SEXP req_shortName, SEXP req_level, SEXP req_step,
        SEXP check)
{

    const char *file          = NULL;
    char *sn            = NULL;
    int        nfiles         = length(gribfile);

    int        *req_stepptr   = INTEGER(req_step);
    int        *req_levelptr  = INTEGER(req_level);
    int        *checkptr      = INTEGER(check);


    /* Inde s is used to iterate over steps, (i,j) over grid positions, 
     * k is a general loop index, f to loop over files. */
    int f, i, j, k, m, s, t;
    int nprotected = 0;

    /* Dummy variables, will be resized later on */
    SEXP rval, rtimes, rdates, rsteps, rlon, rlat, rlevtype, rlev, rmember;
    double *rstepsptr   = NULL, *rlonptr   = NULL, *rlatptr    = NULL;
    double *rtimesptr   = NULL, *rdatesptr = NULL, *rvalptr    = NULL;
    double *rlevtypeptr = NULL, *rlevptr   = NULL, *rmemberptr = NULL;

    /* String types */
    SEXP rshortName, rtypeOfLevel;
    
    double lat, lon, value;      /* Will take up data later on */
    double missingValue = 1e+20; /* Missing value */

    /* Variables for ecCodes */
    FILE* grb = NULL;               /* File connection handler */
    codes_handle   *msg   = NULL;   /* Grib message handler */
    grib_iterator* iter   = NULL;   /* Grib iterator */
    codes_index*   grbidx = NULL;   /* Grib index */
    int err      = 0;               /* Error flag variable */
    int ret      = 0;               /* Index return value */
    int msgcount = 0;               /* Message counter (flag) */
    int totalmsgcount = 0;          /* Total message count */
    double *grbinfo = NULL;         /* Used to store grib information */

    int    *grbcheck = NULL;
    grbcheck = malloc(sizeof(int) * nfiles);


    /* Cout messages we have to read */
    grbidx = codes_index_new(0, "level,step,shortName", &ret);
    for (f = 0; f < nfiles; f++) {

        /* Indexing the current file (extract file name, create grib index,
         * append file to index) */
        file = CHAR(STRING_ELT(gribfile, f));
        ret  = codes_index_add_file(grbidx, file);
        if ( ret ) { Rprintf("error: %s\n", codes_get_error_message(ret)); exit(ret); }
    }

    /* Filtering index */
    codes_index_select_long(grbidx, "step",  req_stepptr[0]);
    codes_index_select_long(grbidx, "level", req_levelptr[0]);
    sn = CHAR(STRING_ELT(req_shortName,0));
    codes_index_select_string(grbidx, "shortName", sn);
    while (( msg = codes_handle_new_from_index(grbidx, &err)) != NULL ) { ++totalmsgcount; }
    codes_index_delete(grbidx);

    //if ( checkptr[0] ) {
    //    for (f = 0; f < nfiles; f++) {
    //        /* Indexing the current file (extract file name, create grib index,
    //         * append file to index) */
    //        grbidx = codes_index_new(0, "step", &ret);
    //        file = CHAR(STRING_ELT(gribfile, f));
    //        Rprintf("Checking %s\r", file);
    //        ret  = codes_index_add_file(grbidx, file);
    //        if ( ret ) { Rprintf("error: %s\n", codes_get_error_message(ret)); exit(ret); }

    //        /* Perform some grib file checks (checking content to be unique and
    //         * extracting the steps to be read) */
    //        _getgrib_check_grib_fields_(file, unique_keys, &grbcheck[f]);

    //        /* Problem? */
    //        if ( grbcheck[f] != 0 ) {
    //            Rprintf("Problem: grib check failed for \"%s\"\n", file); exit(9);
    //        }

    //        /* Release grib indes */
    //        codes_index_delete(grbidx);
    //    }
    //    Rprintf("\nAll %d independent grib file checks look ok\n", nfiles);
    //}

    /* Looping over the messages and read data from grib file */
    long ostep, oN;
    long odate, otime;
    long olevtype, olev, omember;
    long koffset, Ni = -999., Nj = -999.;
    int storelonlat = 1;

    size_t charlen = 20;
    char oshortName[20];
    char otypeOfLevel[20];

    /* I am using a set of grib keys to check whether or not the
     * different fields I am reading do contain the same information,
     * namely the domain size, resolution, and variable. If they do
     * not match the script will stop (before we get strange output) */

    /* Cout messages we have to read */
    grbidx = codes_index_new(0, "level,step,shortName", &ret);
    for (f = 0; f < nfiles; f++) {

        /* Indexing the current file (extract file name, create grib index,
         * append file to index) */
        file = CHAR(STRING_ELT(gribfile, f));
        ret  = codes_index_add_file(grbidx, file);
        if ( ret ) { Rprintf("error: %s\n", codes_get_error_message(ret)); exit(ret); }
    }

    /* Filtering index */
    codes_index_select_long(grbidx, "step",  req_stepptr[0]);
    codes_index_select_long(grbidx, "level", req_levelptr[0]);
    sn = CHAR(STRING_ELT(req_shortName,0));
    codes_index_select_string(grbidx, "shortName", sn);

    /* Looping over the files */
    m = 0;
    while (( msg = codes_handle_new_from_index(grbidx, &err)) != NULL ) {
        
        /* Load grid dimension once */
        if ( Ni < 0. ) {
            /* First field read, extract reference field information */
            Rprintf("Reading grib field info (reference)\n");
            grbinfo = _getgrib_get_grib_info_(msg);
            /* Reading grid dimension (only once) */
            codes_get_long(msg, "Ni", &Ni);
            codes_get_long(msg, "Nj", &Nj);
            /* Create return array which takes up the steps */
            rsteps      = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rstepsptr   = REAL(rsteps);
            rdates      = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rdatesptr   = REAL(rdates);
            rtimes      = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rtimesptr   = REAL(rtimes);
            rlevtype    = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rlevtypeptr = REAL(rlevtype);
            rlev        = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rlevptr     = REAL(rlev);
            rmember     = PROTECT(allocVector(REALSXP, totalmsgcount)); ++nprotected;
            rmemberptr  = REAL(rmember);

            rshortName   = PROTECT(allocVector(STRSXP, totalmsgcount)); ++nprotected;
            rtypeOfLevel = PROTECT(allocVector(STRSXP, totalmsgcount)); ++nprotected;
            /* Create return matrix which takes up the data */
            rval    = PROTECT(Rf_alloc3DArray(REALSXP, Ni, Nj, totalmsgcount)); ++nprotected;
            rvalptr = REAL(rval);
            /* Create return matrices which will take up longitudes/latitudes */
            rlon    = PROTECT(allocMatrix(REALSXP, Ni, Nj)); ++nprotected;
            rlonptr = REAL(rlon);
            rlat    = PROTECT(allocMatrix(REALSXP, Ni, Nj)); ++nprotected;
            rlatptr = REAL(rlat);
        } else {
            _getgrib_check_grib_info_(msg, file, grbinfo);
        }

        /* Reading the daaataaa! */
        iter = codes_grib_iterator_new(msg, 0, &err);
        if ( err != CODES_SUCCESS ) CODES_CHECK(err, 0);
 
        /* Loop on all the lat/lon/values. */
        k = 0;
        koffset =  m * Ni * Nj; /////// + s * Ni * Nj;
        while( codes_grib_iterator_next(iter, &lat, &lon, &value) ) {
            /* Store data, longitude, and latitude (once) */
            rvalptr[koffset + k] = value;
            if ( storelonlat ) {
                rlonptr[k] = lon;
                rlatptr[k] = lat;
            }
            k++;
        }

        /* Setting step, initial date, and initial time */
        codes_get_long(msg, "Ni", &Ni);
        codes_get_long(msg, "dataDate", &odate);
        codes_get_long(msg, "dataTime", &otime);
        codes_get_long(msg, "step",     &ostep);
        codes_get_long(msg, "indicatorOfTypeOfLevel", &olevtype);
        codes_get_long(msg, "level",                  &olev);
        codes_get_long(msg, "perturbationNumber",     &omember);
        rtimesptr[m] = otime;
        rdatesptr[m] = odate;
        rstepsptr[m] = ostep;
        rlevtypeptr[m] = olevtype;
        rlevptr[m]     = olev;
        rmemberptr[m]  = omember;

        /* Loading string values */
        codes_get_string(msg, "shortName",  oshortName, &charlen);
        SET_STRING_ELT(rshortName,  m, Rf_mkChar(oshortName));
        ///codes_get_string(msg, "typeOfLevel",  otypeOfLevel, &charlen);
        ///SET_STRING_ELT(rtypeOfLevel,  m, Rf_mkChar(otypeOfLevel));

        storelonlat = 0;
        codes_handle_delete(msg);

        ++m; /* Increasing message counter */
    }
    Rprintf("\n");

    /* Release grib indes */
    codes_index_delete(grbidx);
 
    //return R_NilValue;
    /* Construct result list from variables containing the results */
    SEXP res   = PROTECT(allocVector(VECSXP,    10));  ++nprotected;
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 10));  ++nprotected;
    SET_STRING_ELT(names,  0, Rf_mkChar("steps"));
    SET_STRING_ELT(names,  1, Rf_mkChar("dates"));
    SET_STRING_ELT(names,  2, Rf_mkChar("times"));
    SET_STRING_ELT(names,  3, Rf_mkChar("lons"));
    SET_STRING_ELT(names,  4, Rf_mkChar("lats"));
    SET_STRING_ELT(names,  5, Rf_mkChar("data"));
    SET_STRING_ELT(names,  6, Rf_mkChar("shortName"));
    SET_STRING_ELT(names,  7, Rf_mkChar("indicatorOfTypeOfLevel"));
    SET_STRING_ELT(names,  8, Rf_mkChar("level"));
    SET_STRING_ELT(names,  9, Rf_mkChar("member"));
    //SET_STRING_ELT(names, 10, Rf_mkChar("typeOfLevel"));
    Rf_setAttrib(res, R_NamesSymbol, names);

    SET_VECTOR_ELT(res, 0, rsteps);
    SET_VECTOR_ELT(res, 1, rdates);
    SET_VECTOR_ELT(res, 2, rtimes);
    SET_VECTOR_ELT(res, 3, rlon);
    SET_VECTOR_ELT(res, 4, rlat);
    SET_VECTOR_ELT(res, 5, rval);
    SET_VECTOR_ELT(res, 6, rshortName);
    SET_VECTOR_ELT(res, 7, rlevtype);
    SET_VECTOR_ELT(res, 8, rlev);
    SET_VECTOR_ELT(res, 9, rmember);
    //SET_VECTOR_ELT(res, 10, rtypeOfLevel);

    /* Release allocated memory and unprotect variables */
    UNPROTECT(nprotected);

    /* Return results vector */
    return res;

}

