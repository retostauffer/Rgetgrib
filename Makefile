

packageversion:=$(shell cat getgrib/DESCRIPTION | egrep "^Version" | sed 's/Version://g')

package: SHELL:=/bin/bash
package: getgrib
	##:export PKG_FCFLAGS="-static-libgfortran -I/usr/local/include"
	##:export PKG_LIBS="-L/usr/local/lib -leccode"
	##:export PKG_FCFLAGS="-L/usr/local/lib -I/usr/local/include"
	##:export PKG_FCLIBS="-L/usr/local/lib"
	##:export LD_LIBRARY_PATH=/usr/local/lib:$${LD_LIBRARY_PATH}
	R CMD build --no-build-vignettes getgrib
	R CMD INSTALL getgrib_$(shell printf "%s"${packageversion}).tar.gz

readme: README.md
	pandoc README.md -o README.pdf
