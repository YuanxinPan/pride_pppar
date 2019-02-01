#!/bin/csh -f
# generate leap.sec file from sopac leap.sec file
#
wget -O temp -nv ftp://garner.ucsd.edu/pub/gamit/tables/leap.sec
set inp = "temp"

echo "+leap sec" >! leap.sec
awk 'BEGIN{lp=21}{if(index($0,"!")!=0){printf("%6d%4d\n",int($1-2400001.0),lp);lp=lp+1}}' $inp >> leap.sec
echo "-leap sec" >> leap.sec

rm -f temp
