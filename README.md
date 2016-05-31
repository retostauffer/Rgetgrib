
SHORT
=====
WHAT:       A small R-package
FOR WHAT:   R/grib file interaction (using grib_api fortran code)
WHO:        Reto Stauffer
WHEN:       End of Mai 2016

THIS IS AN R-PACKAGE USING FORTRAN CODE
=======================================

The goal was to provide a fast and efficient interface between
R and the ECMWF grib API library.

Getgrib uses some fortran scripts (see _src_ directory) to extract
the data from the grib files. Originally designed for an application
of Sascha - nearest neighbor search. 

INSTALLATION NOTES
==================
A proper grib API installation is required to build the fortran
code. Furthermore, some paths and flags have to be set properly. 
I was using the _make.sh_ file which should be included in this
repository, but - of course - contains some paths fitting my
machine. Please check the paths there and then do something
similar as this (some bash shell code):

   #!/bin/bash
   version=`cat getgrib/DESCRIPTION | grep 'Version:' | awk '{print $2}'`
   export PKG_FCFLAGS="-static-libgfortran -L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"
   export PKG_LIBS="-L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"
   R CMD build --no-build-vignettes getgrib
   R CMD INSTALL getgrib_${version}.tar.gz



