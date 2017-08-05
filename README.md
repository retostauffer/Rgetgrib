
# SHORT

* WHAT:       A small R-package
* FOR WHAT:   R/grib file interaction (using grib_api fortran code)
* WHO:        Reto Stauffer
* WHEN:       End of Mai 2016

# R Package to Directly Read From Grib Files

Once upon a time I was working with Sascha on a small problem on how to
efficiently read grib data in R. There is the ``raster`` package which offers some
functionality, however, the ``raster`` package is neither quick nor does it
provide the (often) required meta information of the grib messages or is able
to read data from rotated grids (like COSMO).  This was the beginning of this
``getgrib`` package which offers some grib handling functionalities using the
``ECMWF GRIB API`` (see installation notes).

Over the time the R package getgrib got some updates and
extensions, and some methods have been replaced and/or removed by better,
faster, or more flexible methods (written in C/Fortran/R). 

The package also contains a vignette ``overview.pdf`` which shows an
overview over the different functionalities which are currently implemented.


# Installation Notes

A proper grib API installation is required to build the Fortran
and C shared object files included in the package.
Furthermore, some paths and flags have to be set properly. 
I was using the ``make.sh`` file which should be included in this
repository, but---of course---contains some paths customized to my
machine and OS. Please check the paths there and then do something
similar as this (some bash shell code):

```
#!/bin/bash
## This line simply extracts the current package version
## out of the R package DESCRIPTION file
version=`cat getgrib/DESCRIPTION | grep 'Version:' | awk '{print $2}'`

## Setting required R flags for the compilers (IMPORTANT)
export PKG_FCFLAGS="-static-libgfortran -L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"
export PKG_LIBS="-L/usr -I/usr/include -lgrib_api_f90 -lgrib_api"

## Build and install package
R CMD build --no-build-vignettes getgrib
R CMD INSTALL getgrib_${version}.tar.gz
```

# Bilinear Interpolation

There is a function called ``bilinear`` which performs bilinear interpolation
on grib files (deterministic and ensemble grib files) using some C-backend
code based on the ``GRIB_API`` (wherefore the compiler flags are required).
Code written somewhen in spring 2017 by Reto Stauffer.

Method in ``bilinearlist.c``  used by ``bilinear(...)`` modified, is now able
to interpolate grib messages without ``perturbationNumber`` in the message header.
A ``perturbationNumber = 0`` will be returned if not found.

# Reading Grib Data

Another nice set of functions is provided by ``getdata`` and ``gribdata2raster``
(please check the package manual pages or the vignette for more information).
These functions allow to load full grids and convert them to ``RasterLayer``
objects if you wanna plot them.

















