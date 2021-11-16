#!/bin/bash

main() {
set -e

[ -z "$DART" ] && echo "ERROR: Must set DART environment variable" && exit 9

CONVERTER="NCEP/ascii_to_obs"
LOCATION=threed_sphere
source $DART/build_templates/buildconvfunctions.sh

programs=( \
dwl_to_obs \
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
