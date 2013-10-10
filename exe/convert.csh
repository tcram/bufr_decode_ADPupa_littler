#!/bin/csh

# Script to process all BUFR tar files located in "bufrdecodelr/bufrobs"

# Make sure all tar files are in uncompressed form for script to work.

# !! Edit procdir directory to reflect your local system !!

set procdir=$home/bufrdecodelr
set CPLAT=other

# !!! Uncomment the following for linux !!!
set CPLAT=linux

if($CPLAT =~ "linux") then 
 set f90=g95    ## set f90=compiler_name, ie for ibm-sp f90=xlf
 cd $procdir/grabbufr
 $f90 -o grabbufr grabbufr.f spbufr.f
endif

cd $procdir/bufrobs

set z=0

foreach file (gdasupaobs.*????.tar)

 tar -xvf $file

end

foreach dir (upaobs.*????)

 set date=`echo $dir | awk -F. '{print $2}'` 

  foreach hh ("00" "06" "12" "18") 

  set datehh=$date$hh
  set hour=$hh"z"

  echo $datehh
  echo $hour

if($CPLAT =~ "linux")then
  cp $procdir/grabbufr/grabbufr $procdir/bufrobs/$dir
  cd $dir
  wc -c gdas.aircar.t$hour.$date.bufr | ./grabbufr gdas.aircar.t$hour.$date.bufr aircar.t$hour.le
  mv aircar.t$hour.le gdas.aircar.t$hour.$date.bufr 
  wc -c gdas.aircft.t$hour.$date.bufr | ./grabbufr gdas.aircft.t$hour.$date.bufr aircft.t$hour.le
  mv aircft.t$hour.le gdas.aircft.t$hour.$date.bufr 
  wc -c gdas.adpupa.t$hour.$date.bufr | ./grabbufr gdas.adpupa.t$hour.$date.bufr adpupa.t$hour.le
  mv adpupa.t$hour.le gdas.adpupa.t$hour.$date.bufr 
  wc -c gdas.satwnd.t$hour.$date.bufr | ./grabbufr gdas.satwnd.t$hour.$date.bufr satwnd.t$hour.le
  mv satwnd.t$hour.le gdas.satwnd.t$hour.$date.bufr
  cd ..
endif
 
  $procdir/exe/bufr_aircar2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircar.t$hour.$date.bufr $datehh
  $procdir/exe/bufr_craft2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircft.t$hour.$date.bufr $datehh 
  $procdir/exe/bufr_upa2ob.x $procdir/bufrobs/upaobs.$date/gdas.adpupa.t$hour.$date.bufr $datehh 
  $procdir/exe/bufr_sat2ob.x $procdir/bufrobs/upaobs.$date/gdas.satwnd.t$hour.$date.bufr $datehh 

  echo Airca$datehh.obs >files.txt
  echo Aircraft$datehh.obs >>files.txt
  echo Satob$datehh.obs >>files.txt
  echo Upper$datehh.obs >>files.txt

  $procdir/exe/runob2lit_imd_obs.x files.txt $datehh

  rm Airca$datehh.obs 
  rm Aircraft$datehh.obs
  rm Satob$datehh.obs 
  rm Upper$datehh.obs

  rm files.txt

  mv *OBS* $procdir/lrobs
  end
   
end
