#!/bin/bash

main() {
set -e

[ -z "$DART" ] && echo "ERROR: Must set DART environment variable" && exit 9

source $DART/build_templates/buildconvfunctions.sh
CONVERTER=gps
LOCATION=threed_sphere
LIBRARIES="../../NCEP/prep_bufr/lib/bufrlib.a"  

programs=( \
convert_cosmic_gps_cdf \
convert_cosmic_ionosphere \
convert_gpsro_bufr \
obs_sequence_tool \
advance_time
)

# build arguments
arguments "$@"

# clean the directory
\rm -f *.o *.mod Makefile .cppdefs

# build and run preprocess before making any other DART executables
buildpreprocess

# build 
buildconv


# clean up
\rm -f *.o *.mod

}

main "$@"
