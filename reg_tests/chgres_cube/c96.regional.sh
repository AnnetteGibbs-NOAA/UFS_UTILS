#!/bin/bash

#-----------------------------------------------------------------------------
# Invoke chgres to create C96 regional coldstart files, including a lateral
# boundary file, using FV3 gaussian nemsio files as input.  The 
# coldstart files are then compared to baseline files using the 'nccmp' 
# utility.  
#
# This script is run by the machine specific driver script.
#-----------------------------------------------------------------------------

set -x

export DATA=$OUTDIR/c96_regional
rm -fr $DATA

export FIXfv3=${HOMEreg}/fix/C96.regional
export OROG_FILES_TARGET_GRID="C96_oro_data.tile7.nc"
export COMIN=${HOMEreg}/input_data/fv3.nemsio
export ATM_FILES_INPUT=gfs.t12z.atmf000.nemsio
export SFC_FILES_INPUT=gfs.t12z.sfcf000.nemsio
export VCOORD_FILE=${HOMEufs}/fix/am/global_hyblev.l64.txt
export REGIONAL=1
export HALO_BLEND=0
export HALO_BNDY=4

export CDATE=2019070412

export OMP_NUM_THREADS_CH=${OMP_NUM_THREADS:-1}

NCCMP=${NCCMP:-$(which nccmp)}

DEBUG_MODE=${DEBUG_MODE:-'false'}

#-----------------------------------------------------------------------------
# Invoke chgres program.
#-----------------------------------------------------------------------------

echo "Starting at: " `date`

${HOMEufs}/ush/chgres_cube.sh

iret=$?
if [ $iret -ne 0 ]; then
  set +x
  echo "<<< C96 REGIONAL DEBUG TEST FAILED. <<<"
  exit $iret
fi

echo "Ending at: " `date`

#-----------------------------------------------------------------------------
# Compare output from chgres to baseline set of data.
#-----------------------------------------------------------------------------

cd $DATA

mv out.sfc.tile7.nc out.sfc.tile1.nc
mv out.atm.tile7.nc out.atm.tile1.nc

if [ "$DEBUG_MODE" = 'true' ]; then
  BASELINE_DIR=c96_regional.debug
else
  BASELINE_DIR=c96_regional
fi
BASELINE_DATA=$HOMEreg/baseline_data/$BASELINE_DIR

test_failed=0
for files in *.nc
do
  if [ -f $files ]; then
    echo CHECK $files
    $NCCMP -dmfqS $files $BASELINE_DATA/$files
    iret=$?
    if [ $iret -ne 0 ]; then
      test_failed=1
    fi
  fi
done

set +x
if [ $test_failed -ne 0 ]; then
  echo "<<< C96 REGIONAL DEBUG TEST FAILED. >>>"
  if [ "$UPDATE_BASELINE" = "TRUE" ]; then
    $HOMEufs/reg_tests/update_baseline.sh $HOMEreg $BASELINE_DIR $commit_num
  fi
else
  echo "<<< C96 REGIONAL DEBUG TEST PASSED. >>>"
fi

exit 0
