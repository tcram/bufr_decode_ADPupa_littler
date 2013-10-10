#!/bin/csh

# Script to process all BUFR tar files located in "bufrdecodelr/bufrobs"


# !! Edit procdir directory to reflect your local system !!

set procdir=`pwd`/..

cd $procdir/bufrobs

foreach file (gdasupaobs.*????.tar*)

# gdasupaobs.yyyymmdd.tar.gz
   if ("$file" =~ *.gz)  then
       tar -xvzf $file
   else
       tar -xvf $file
   endif
end

foreach dir (upaobs.*????)

 set date=`echo $dir | awk -F. '{print $2}'` 

  foreach hour (`ls -1 $dir | cut -d. -f3 | sort | uniq`)
      set hh=`echo $hour | sed 's|[A-Za-z]||g'`
    
      set datehh=$date$hh
    
      echo $datehh
      echo $hour
    
      echo "$procdir/exe/bufr_aircar2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircar.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_aircar2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircar.$hour.$date.bufr $datehh
      echo "$procdir/exe/bufr_craft2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircft.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_craft2ob.x $procdir/bufrobs/upaobs.$date/gdas.aircft.$hour.$date.bufr $datehh 
      echo "$procdir/exe/bufr_upa2ob.x $procdir/bufrobs/upaobs.$date/gdas.adpupa.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_upa2ob.x $procdir/bufrobs/upaobs.$date/gdas.adpupa.$hour.$date.bufr $datehh 
      echo "$procdir/exe/bufr_sat2ob.x $procdir/bufrobs/upaobs.$date/gdas.satwnd.$hour.$date.bufr $datehh"
      $procdir/exe/bufr_sat2ob.x $procdir/bufrobs/upaobs.$date/gdas.satwnd.$hour.$date.bufr $datehh 
    
      echo Airca$datehh.obs >files.txt
      echo Aircraft$datehh.obs >>files.txt
      echo Satob$datehh.obs >>files.txt
      echo Upper$datehh.obs >>files.txt
    
      echo "$procdir/exe/runob2lit_imd_obs.x files.txt $datehh"
      $procdir/exe/runob2lit_imd_obs.x files.txt $datehh
    
      rm Airca$datehh.obs 
      rm Aircraft$datehh.obs
      rm Satob$datehh.obs 
      rm Upper$datehh.obs
    
      rm files.txt
    
      mv *OBS* $procdir/lrobs
  end
   
end
