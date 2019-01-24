#!/bin/bash
# Calculate coordinates using rnx2rtkp
# input: directory containing rinex data

cd $1
# Sites
sit=($(ls -1 *o | cut -c1-4))

# Check if broadcast ephemeris exist
tmp=$(ls -1 *o | awk '{print $0;exit}')
doy=$(echo $tmp | cut -c5-7)
yr=$(echo $tmp | cut -c10-11)
if [ -e brdc${doy}0.${yr}n.Z ]; then
    gunzip brdc${doy}0.${yr}n.Z
fi
if [ ! -e brdc${doy}0.${yr}n ]; then
    wget -nv -nc -t 3 --connect-timeout=10 --read-timeout=60 ftp://cddis.gsfc.nasa.gov/pub/gps/data/daily/20$yr/$doy/${yr}n/brdc${doy}0.${yr}n.Z
    if [ "$?" -ne "0" ]; then
        echo "n file download error"
        exit 1
    fi
    gunzip brdc${doy}0.${yr}n.Z
fi

# Site by site
rm -f sit.xyz
for ss in ${sit[@]}; do
    echo $ss
    # rnx2rtkp
    rnx2rtkp -o outxyz -p 0 -ti 3600 -e ${ss}${doy}0.${yr}o brdc${doy}0.${yr}n
    rnx2rtkp -o outblh -p 0 -ti 3600 ${ss}${doy}0.${yr}o brdc${doy}0.${yr}n
    teqc -R -E -O.obs "L1L2P1P2C1C2" -n_GLONASS 30 ${ss}${doy}0.${yr}o >obs
    rm -f ${ss}${doy}0.${yr}o
    mv -f obs ${ss}${doy}0.${yr}o
    # collect coordinates
    awk -v nam=$ss '{if(substr($0,1,1)!="%"){printf(" %16.6f%16.6f%10s\n",$4,$3,nam);exit}}' outblh >>blh.xyz
    awk -v nam=$ss '{if(substr($0,1,1)!="%"){printf(" %s%16.4f%16.4f%16.4f\n",nam,$3,$4,$5);exit}}' outxyz >>sit.xyz
done
rm -f outxyz outblh
cd ..
