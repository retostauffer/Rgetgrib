
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <grib_api.h>

/* Search for closest north east grid point */
int closest_northeast(double stnlon, double stnlat, int n, double *lons, double *lats, double *dist) {
   static int res = -9;
   int i;
   for ( i = 0; i < n; i++ ) {
      /* Gridpoint not onrtheast from station location */
      if ( lons[i] <= stnlon || lats[i] <= stnlat ) continue;
      if ( res < 0 ) { res = i; continue; }
      if ( dist[i] < dist[res] ) { res = i; }
   }
   Rprintf("    Closest NE:  %10.5f %10.5f\n",lons[res],lats[res]);
   return res;
}
/* Search for closest south east grid point */
int closest_southeast(double stnlon, double stnlat, int n, double *lons, double *lats, double *dist) {
   static int res = -9;
   int i;

   for ( i = 0; i < n; i++ ) {
      if ( lons[i] <= stnlon || lats[i] > stnlat ) continue;
      if ( res < 0 ) { res = i; continue; }
      if ( dist[i] < dist[res] ) { res = i; }
   }
   Rprintf("    Closest SE:  %10.5f %10.5f\n",lons[res],lats[res]);
   return res;
}
/* Search for closest south west grid point */
int closest_southwest(double stnlon, double stnlat, int n, double *lons, double *lats, double *dist) {
   static int res = -9;
   int i;

   for ( i = 0; i < n; i++ ) {
      if ( lons[i] > stnlon || lats[i] > stnlat ) continue;
      if ( res < 0 ) { res = i; continue; }
      if ( dist[i] < dist[res] ) { res = i; }
   }
   Rprintf("    Closest SW:  %10.5f %10.5f\n",lons[res],lats[res]);
   return res;
}
/* Search for closest north west grid point */
int closest_northwest(double stnlon, double stnlat, int n, double *lons, double *lats, double *dist) {
   static int res = -9;
   int i;

   for ( i = 0; i < n; i++ ) {
      if ( lons[i] > stnlon || lats[i] <= stnlat ) continue;
      if ( res < 0 ) { res = i; continue; }
      if ( dist[i] < dist[res] ) { res = i; }
   }
   Rprintf("    Closest NW:  %10.5f %10.5f\n",lons[res],lats[res]);
   return res;
}
/* Computes euclidean distance */
double compute_distance(double x1,double x2,double y1,double y2) {
   static double res;
   res = sqrt(pow(x1-x2,2.) + pow(y1-y2,2.));
   return res;
}
/* Given station location stnlon and stnlat this function searches for the
   4 nearest grid points in quadrants. Returns an integer vector with 4
   values containing indizes for the grid points (NE,SE,SW,NW) */
int * get_surrounding_grid_point_indizes(double stnlon,double stnlat,int n,
         double * lons,double * lats) {

   int i;
   static int res[4];
   double dist[n];

   /* Compute distance to each grid point */
   for ( i=0; i<n; i++ ) { dist[i] = compute_distance(stnlon,lons[i],stnlat,lats[i]); }

   /* Searching for nearest neighbor gridpoints, quadrants, regular_ll! */
   Rprintf("    Station:     %10.5f %10.5f\n",stnlon,stnlat);
   res[0] = closest_northeast(stnlon,stnlat,n,lons,lats,dist);
   res[1] = closest_southeast(stnlon,stnlat,n,lons,lats,dist);
   res[2] = closest_southwest(stnlon,stnlat,n,lons,lats,dist);
   res[3] = closest_northwest(stnlon,stnlat,n,lons,lats,dist);

   return res;
}
/* Compute weights for interpolation */
double * get_interpolation_weights(double stnlon, double stnlat,
         double * lons, double * lats, int * neighbors) {
   static double res[4];
   double dist[4];
   int i;
   for ( i = 0; i < 4; i++ ) {
      Rprintf("  %d  ",neighbors[i]);
      dist[i] = compute_distance(stnlon,lons[neighbors[i]],stnlat,lats[neighbors[i]]);
      Rprintf("  %f \n",dist[i]);
   }
   return res;
}

/* -------------------- MAIN FUNCTION ------------------- */
SEXP grib_bilinear_interpolation(SEXP gribfile, SEXP statnr, SEXP statlon, SEXP statlat,
      SEXP verbose)
{

   int i, nprotected=0, err=0;
   int nstat = length(statnr);

   double *statnrptr  = REAL(statnr);
   double *statlonptr = REAL(statlon);
   double *statlatptr = REAL(statlat);
   int    *verboseptr = INTEGER(verbose);

   /* Variables to store data from grib messages */
   size_t values_len = 0;
   double *values = NULL, *lons = NULL, *lats = NULL;

   /* Open grib file handler */
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
   if ( verboseptr[0] >= 2 ) {
      Rprintf(" * Station list output\n");
      for ( i = 0; i < nstat; i++ ) {
         Rprintf("   %5.0f  %10.5f %10.5f\n",statnrptr[i],statlonptr[i],statlatptr[i]);
      }
   }


   /* Open grib file connection */
   h = grib_handle_new_from_file(0,in,&err);
   

   /* Getting size of the message */
   GRIB_CHECK(grib_get_size(h,"values",&values_len),0);
   if ( verboseptr[0] >= 2 ) { Rprintf(" * Grib message size: %d\n",values_len); }
   values = malloc(values_len*sizeof(double)); /* values    */
   lons   = malloc(values_len*sizeof(double)); /* longitude */
   lats   = malloc(values_len*sizeof(double)); /* latitude  */

   /* Getting data values */
   GRIB_CHECK(grib_get_double_array(h,"values",     values, &values_len), 0);
   GRIB_CHECK(grib_get_double_array(h,"longitudes", lons,   &values_len), 0);
   GRIB_CHECK(grib_get_double_array(h,"latitudes",  lats,   &values_len), 0);


   /* Get interpolation for station */
   for ( i = 0; i < nstat; i++ ) {
      /* Get interpolation */
      int *x;
      x = get_surrounding_grid_point_indizes(statlonptr[i],statlatptr[i],values_len,lons,lats);
      double *w;
      w = get_interpolation_weights(statlonptr[i],statlatptr[i],lons,lats,x);
      Rprintf(" ---------- %f\n",x);
   }

   for ( i = 0; i < 20; i++ ) {
      Rprintf("    %5d: %10.5f\n",i,values[i]);
   }


//  /* Loop on all the messages in a file.*/
//  while ((h = grib_handle_new_from_file(0,in,&err)) != NULL ) {
//    /* Check of errors after reading a message. */
//    if (err != GRIB_SUCCESS) GRIB_CHECK(err,0);  


   UNPROTECT(nprotected);
   return rval;
}

