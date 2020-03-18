#!/bin/sh
#  ------------------------------------------------------------------------
#  This script will make executables to extract data from ADP BUFR
#  input files, write the data into a basic text file, and convert
#  the text files into LITTLE_R format.  It is used to
#  extract data from these kinds of files:
#      gdas.adpupa.tHHz.YYYYMMDD.bufr 
#      gdas.aircft.tHHz.YYYYMMDD.bufr
#      gdas.satwnd.tHHz.YYYYMMDD.bufr 
#      gdas.aircar.tHHz.YYYYMMDD.bufr
#
#  bufr_upa2ob.x:       decodes ADPUPA observations and writes to a text file
#  bufr_aircar2ob.x:    decodes AIRCAR observations and writes to a text file
#  bufr_craft2ob.x:     decodes AIRCFT observations and writes to a text file
#  bufr_sat2ob.x:       decodes SATWND observations and writes to a text file
#  runob2lit_imd_obs.x: reads observation text files and converts to little_r format
#  dumpbufr.x:          used to dump all contents of a BUFR file.
#  ------------------------------------------------------------------------
 
set -eua
 
#  ------------------------------------------------------------------------
#  CPLAT - platform type (linux,sgi,aix,sun)
#  ------------------------------------------------------------------------
 
CPLAT=linux
SRC=../src
LIB=/path/to/BUFRLIB
EXE=../exe
INSTALL=.
 
cflag=""
fflag=""

if [ $CPLAT = linux ]
then
   export FC=gfortran
   export CC=gcc
   fflag=" -O3 -DUNDERSCORE -fno-second-underscore -w"
   cflag=" -O3 -DUNDERSCORE -w"
fi

#  Compile the decode programs
#  ---------------------------------------
 
echo "Compiling bufr_configdecode_ADPupa programs..."
$FC $fflag -c $SRC/dumpbufr.f

$FC $fflag -c $SRC/bufr_upa2ob.f
$FC $fflag -c $SRC/bufr_aircar2ob.f
$FC $fflag -c $SRC/bufr_craft2ob.f
$FC $fflag -c $SRC/bufr_sat2ob.f

$FC $fflag -c $SRC/runob2lit_imd_obs.f
 
#  link and load the executables
#  -----------------------------

echo "Linking..."
$FC $fflag -o $EXE/dumpbufr.x dumpbufr.o $LIB/bufrlib.a

$FC $fflag -o $EXE/bufr_upa2ob.x bufr_upa2ob.o $LIB/bufrlib.a
$FC $fflag -o $EXE/bufr_aircar2ob.x bufr_aircar2ob.o $LIB/bufrlib.a
$FC $fflag -o $EXE/bufr_craft2ob.x bufr_craft2ob.o $LIB/bufrlib.a
$FC $fflag -o $EXE/bufr_sat2ob.x bufr_sat2ob.o $LIB/bufrlib.a

$FC $fflag -o $EXE/runob2lit_imd_obs.x runob2lit_imd_obs.o

#  clean up
#  --------

rm -f *.o

echo "Finished."
