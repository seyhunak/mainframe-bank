#!/bin/sh
# Build every program in the bank from source.
#
#   bin/BUILD          build everything
#   bin/BUILD clean    remove the build directory first
#
# The compiler is GnuCOBOL 3.x. Sources are free format, so -free; the
# copybooks live in src/copy and are found with -I.
set -e
cd "$(dirname "$0")/.."

COBC=${COBC:-cobc}
COBFLAGS="-x -free -I src/copy"

if [ "$1" = "clean" ]; then
    rm -rf build
    mkdir -p build
fi
mkdir -p build

echo "building bin/LOADLIB  (dataset bootstrap)"
$COBC $COBFLAGS -o bin/LOADLIB \
    src/init/MBLD001.cbl \
    src/lib/MBDATE.cbl src/lib/MBMONEY.cbl

echo "building bin/TERMBANK (teller terminal)"
$COBC $COBFLAGS -o bin/TERMBANK \
    src/cics/MBANMAIN.cbl \
    src/lib/MBSCRN.cbl src/lib/MBPOSTNG.cbl src/lib/MBMONEY.cbl \
    src/lib/MBDATE.cbl src/lib/MBINTCAL.cbl \
    src/lib/MBLOGN.cbl src/lib/MBPARM.cbl src/lib/MBGL.cbl

echo "building bin/NIGHTEND (night-end batch)"
$COBC $COBFLAGS -o bin/NIGHTEND \
    src/batch/MBJB001.cbl \
    src/lib/MBINTCAL.cbl src/lib/MBPARM.cbl

echo "building tests"
for t in TDATE TMONEY TINTCAL TPOST TLOGN TPARM TJOB; do
    case $t in
        TDATE)   mods="src/lib/MBDATE.cbl" ;;
        TMONEY)  mods="src/lib/MBMONEY.cbl" ;;
        TINTCAL) mods="src/lib/MBINTCAL.cbl" ;;
        TPOST)   mods="src/lib/MBPOSTNG.cbl src/lib/MBDATE.cbl src/lib/MBMONEY.cbl src/lib/MBINTCAL.cbl src/lib/MBGL.cbl src/lib/MBPARM.cbl" ;;
        TLOGN)   mods="src/lib/MBLOGN.cbl" ;;
        TPARM)   mods="src/lib/MBPARM.cbl" ;;
        TJOB)    mods="src/lib/MBINTCAL.cbl src/lib/MBPARM.cbl" ;;
    esac
    $COBC $COBFLAGS -o build/$t test/$t.cbl $mods
done

echo "done"
