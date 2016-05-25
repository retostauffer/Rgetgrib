

# export GRIB_API_LIB="/usr/local"
# export GRIB_API_INCLUDE="/usr/local/include"
# gfortran -c -I/usr/include grib.f95 -o grib.o  
# gfortran grib.o -L${GRIB_API_LIB} -lgrib_api_f90 -lgrib_api -o grib.out
# ./grib.out

export GRIB_API_LIB="/usr/local"
export GRIB_API_INCLUDE="/usr/local/include"

gfortran -c -I/usr/include grib.f95 -o grib.o  
if [ $? -ne 0 ] ; then
   echo 'problems compiling stpe 1'
   exit
fi
gfortran grib.o -L${GRIB_API_LIB} -lgrib_api_f90 -lgrib_api -o grib.out
if [ $? -ne 0 ] ; then
   echo 'problems compiling stpe 2'
   exit
fi
./grib.out
