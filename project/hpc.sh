#!/bin/bash
#PBS -N opensource
#PBS -q batch
#PSB -l nodes=1:ppn=1
#PBS -l walltime=60:00:02
#PBS -o ./output/kinematic/2015/out
#PBS -e ./output/kinematic/2015/err

RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

yr=15
year=20$yr
doy=`echo $PBS_ARRAYID | awk '{printf("%03d",$1)}'`
mon=`jday $doy 20$yr | cut -c1-2`
date=`jday $doy 20$yr | cut -c3-4`

dir=`mktemp -d` && cd $dir

# Check prerequisite
wget --version > /dev/null 2>&1
if [ $? -ne 0 ]; then
    printf "${RED}error:${NC} wget not found\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi

# Check installation
lsq > /dev/null 2>&1
if [ $? -eq 127 ]; then  # command not found
    printf "${RED}error:${NC} ${HOME}/.PRIDE_PPPAR_BIN not in PATH\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi

# Generate control file
config=${PBS_O_WORKDIR}/config_template    # configuration file

sed '/^\*NAME / q' ${config} > ses.ppp
for f in /home/yxpan/data/fcb/${year}/${doy}/rinex/*.${yr}o
do
    site=$(basename $f)
    site=$(cut -c 1-4 <<< $site)
    echo " ${site} K _GMF 9000  7 0.20 .020 .005 .002 3.00 .006 10.00 10.00 10.00 10.00 10.00 10.00 1.000 1.000 1.000" >> ses.ppp
done
echo "-Station used" >> ses.ppp

# Computation
pride_pppar ses.ppp ${year}${mon}${date} ${year}${mon}${date} y

results_dir=${PBS_O_WORKDIR}/results/kinematic/${year}/
# mv back results
[ -d ${results_dir} ] || mkdir -p ${results_dir}
if [ -d ${results_dir}/${doy} ]; then
    tmp=$(mktemp -u)
    mv ${results_dir}/${doy} ${results_dir}/${doy}${tmp:8:11}
fi
mv ${year}/${doy} ${results_dir}/ && cd .. && rm -rf $dir
