#!/bin/bash

set -u

# - Checking version
version=`cat getgrib/DESCRIPTION | grep 'Version:' | awk '{print $2}'`
host=`hostname`

if [ $host == "meteo-data.uibk.ac.at" ] ; then
   export PKG_FCFLAGS="-L/opt/local_libs/lib -I/opt/local_libs/include -lgrib_api_f90 -lgrib_api"
   export PKG_LIBS="-L/opt/local_libs/lib -I/opt/local_libs/include -lgrib_api_f90 -lgrib_api"
else
   export PKG_FCFLAGS="-static-libgfortran -L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"
   export PKG_LIBS="-L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"
fi

printf " BUILDING PACKAGE: getgrib\n"
R CMD build --no-build-vignettes getgrib
if [ $? -ne 0 ] ; then
    printf "\n\nerror building package\n\n"; exit
fi

printf " INSTALLING PACKAGE: getgrib_%s.tar.gz\n" ${version}
R CMD INSTALL getgrib_${version}.tar.gz
if [ $? -ne 0 ] ; then
    printf "\n\nerror installing package\n\n"; exit
fi

