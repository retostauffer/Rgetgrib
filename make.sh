#!/bin/bash
# -------------------------------------------------------------------
# - NAME:        make.sh
# - AUTHOR:      Reto Stauffer
# - DATE:        2017-02-24
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# - DESCRIPTION: Mini script to install the getgrib package.
# -------------------------------------------------------------------
# - EDITORIAL:   2017-02-24, RS: Created file on thinkreto.
# -------------------------------------------------------------------
# - L@ST MODIFIED: 2017-08-05 13:12 on thinkreto
# -------------------------------------------------------------------

# SASCHA VSC # # Load modules
# SASCHA VSC # module purge
# SASCHA VSC # module load  intel/14.0.2 gcc/4.9 intel-mkl/11.3 R/3.3.1
# SASCHA VSC # 
# SASCHA VSC # # - Checking version
# SASCHA VSC # version=`cat DESCRIPTION | grep 'Version:' | awk '{print $2}'`
# SASCHA VSC # 
# SASCHA VSC # export PKG_FCFLAGS="-fPIC -static-libgfortran -L/home/lv70667/bellaire/usr/local/lib -I/home/lv70667/bellaire/usr/local/include -lgrib_api_f90 -lgrib_api"
# SASCHA VSC # export PKG_LIBS="-fPIC -L/home/lv70667/bellaire/usr/local/lib -I/home/lv70667/bellaire/usr/local/include -lgrib_api_f90 -lgrib_api"
# SASCHA VSC # 
# SASCHA VSC # printf " BUILDING PACKAGE: getgrib\n"
# SASCHA VSC # R CMD build --no-build-vignettes ./
# SASCHA VSC # if [ $? -ne 0 ] ; then
# SASCHA VSC #     printf "\n\nerror building package\n\n"; exit
# SASCHA VSC # fi
# SASCHA VSC # 
# SASCHA VSC # printf " INSTALLING PACKAGE: getgrib_%s.tar.gz\n" ${version}
# SASCHA VSC # R CMD INSTALL -l /home/lv70667/bellaire/usr/local/R getgrib_${version}.tar.gz
# SASCHA VSC # if [ $? -ne 0 ] ; then
# SASCHA VSC #     printf "\n\nerror installing package\n\n"; exit
# SASCHA VSC # fi

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
R CMD build getgrib
#R CMD build --no-build-vignettes getgrib
if [ $? -ne 0 ] ; then
    printf "\n\nerror building package\n\n"; exit 9
fi

printf " INSTALLING PACKAGE: getgrib_%s.tar.gz\n" ${version}
R CMD INSTALL getgrib_${version}.tar.gz
if [ $? -ne 0 ] ; then
    printf "\n\nerror installing package\n\n"; exit 9
fi

