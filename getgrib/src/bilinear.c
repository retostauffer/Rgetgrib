
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <grib_api.h>


SEXP grib_bilinear_interpolation(SEXP gribfile, SEXP statnr, SEXP statlon, SEXP statlat)
{

   int i, nprotected=0, err=0;
   int nstat = length(statnr);

   //char   *fileptr    = CHAR(STRING_ELT(file,0));
   double *statnrptr  = REAL(statnr);
   double *statlonptr = REAL(statlon);
   double *statlatptr = REAL(statlat);

   const char *file;
   file = CHAR(STRING_ELT(gribfile,0));
   FILE* in = NULL;
   grib_handle *h = NULL; /* grib handling pointer */

   /* Open grib file */
   in = fopen(file,"r");
   if ( ! in ) {
      Rprintf("[C] Cannot open grib file: %s\n",file);
      SEXP rval = PROTECT(allocVector(REALSXP,1)); ++nprotected;
      double *rvalptr = REAL(rval); rvalptr[0] = -99999.;
      UNPROTECT(nprotected);
      return rval;
   }


   /* Create output variable rval */
   SEXP rval = PROTECT(allocVector(REALSXP,nstat)); ++nprotected;
   double *rvalptr = REAL(rval);
   for ( i = 0; i < nstat; i++ ) {
      rvalptr[i] = 6.6;
   }

   Rprintf("[C] Processing file %s\n",file);
   
   /* Loop over stations, station list */
   Rprintf(" --- Station list ---\n");
   for ( i = 0; i < nstat; i++ ) {
      Rprintf(" %5.0f  %10.5f %10.5f\n",statnrptr[i],statlonptr[i],statlatptr[i]);
   }


   /* Open grib file connection */
   //h = grib_handle_new_from_file(0,in,&err);
   






   UNPROTECT(nprotected);
   return rval;
}

