#!/bin/bash

#Run on hera

#SBATCH -J test
#SBATCH -A fv3-cpu
#SBATCH --open-mode=truncate
#SBATCH -o log
#SBATCH -e log
#SBATCH --nodes=1
#SBATCH -q debug
#SBATCH -t 00:03:00

set -x

source /apps/lmod/lmod/init/bash

module purge
module load intel/18.0.5.274
module load netcdf/4.7.0
module load hdf5/1.10.5
module list

WORK=/scratch2/NCEPDEV/stmp1/George.Gayno/test
rm -fr $WORK
mkdir -p $WORK
cd $WORK

export OMP_NUM_THREADS=8

cat << EOF > ./fort.43
 &setup
  lon_out = 1536
  lat_out = 768
 /
EOF

/scratch1/NCEPDEV/da/George.Gayno/ufs_utils.git/UFS_UTILS/sorc/anal_backfill.fd/a.out

exit
