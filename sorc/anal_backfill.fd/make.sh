#!/bin/bash

set -x
source /apps/lmod/lmod/init/bash

module purge
module load intel/18.0.5.274
module load netcdf/4.7.0
module load hdf5/1.10.5

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load sp/2.0.3
module load ip/3.0.1
module load w3nco/2.0.7
module list

export FCOMP=ifort
export FFLAGS="-O0 -I${NETCDF}/include -check bounds -warn unused -qopenmp"
export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5}/lib -lhdf5 -lhdf5_fortran"


make

exit
