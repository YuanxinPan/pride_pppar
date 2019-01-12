#!/bin/bash

doy=$1
year=$2
ac=$3
yr=`echo $year | cut -c3-4`  ## 18
if [ ! -d $year ]; then
  mkdir $year
fi
cd $year
if [ ! -d $doy ]; then
  mkdir $doy
fi
cd $doy
if [ $ac = "igs" ]; then
  ftp="cddis.gsfc.nasa.gov"
fi
if [ $ac = "whu" ]; then
  ftp="igs.gnsswhu.cn"
fi


wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://${ftp}/pub/gps/data/daily/$year/$doy/${yr}d/[a-zA-Z][a-zA-Z][a-zA-Z]*.${yr}d.Z
if [ "$?" -ne "0" ]; then
  echo "data download error";
  exit 1;
fi
#wget -nv -nc --accept="[a-zA-Z][a-zA-Z][a-zA-Z]*\.11d\.Z" -t 3 --connect-timeout=10 --read-timeout=60 ftp://igs.gnsswhu.cn/pub/gps/data/daily/2011/$doy/11d/
cd ../
cd ../
