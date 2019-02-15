#!/bin/bash
# Test PRIDE-PPPAR

RED='\033[0;31m'
BLUE='\033[1;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check installation
source ${HOME}/.bashrc
lsq > /dev/null 2>&1
if [ $? -eq 127 ]; then  # command not found
    printf "${RED}error:${NC} PRIDE-PPPAR:lsq not found\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi

wk_dir=$(pwd)                  # working directory
# Generate control file
config=$(basename `mktemp -u`)
config=${config/tmp/config}    # configuration file
echo "## Session configure" > $config
echo "Interval = 30"       >> $config
echo "Session time    = -YYYY- -MM- -DD- 00 00 00 86360" >> $config
echo "Rinex directory = ${wk_dir}/data/-YEAR-/-DOY-" >> $config
echo "Sp3 directory   = ${wk_dir}/products" >> $config
echo "Table directory = ${wk_dir}/../table" >> $config
cat config_partial >> $config

mkdir -p products results
# Computation
echo -e "(1) static float"
pride_pppar ${config} 20160101 20160101 N
mv 2016/001 ./results/static-float

echo -e "\n(2) static fixed"
pride_pppar ${config} 20160101 20160101 Y
mv 2016/001 ./results/static-fixed

sed -i 's/\(^ \w\w\w\w\) S/\1 K/' ${config}

echo -e "\n(3) kinematic float"
pride_pppar ${config} 20160101 20160101 N
mv 2016/001 ./results/kinematic-float

echo -e "\n(4) kinematic fixed"
pride_pppar ${config} 20160101 20160101 Y
mv 2016/001 ./results/kinematic-fixed

rm -rf 2016

# Output
printf "${BLUE}::${NC} computation results are put in %s\n" ./results/
printf "${BLUE}::${NC} reference results are in %s\n" ./results_ref/
