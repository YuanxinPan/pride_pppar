#!/bin/bash
# Test PRIDE-PPPAR

RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check prerequisite
perl --version > /dev/null 2>&1
if [ $? -ne 0 ]; then
    printf "${RED}error:${NC} perl not found\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi
wget --version > /dev/null 2>&1
if [ $? -ne 0 ]; then
    printf "${RED}error:${NC} wget not found\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi

# Check installation
source ${HOME}/.bashrc
lsq #> /dev/null 2>&1
if [ $? -eq 127 ]; then  # command not found
    printf "${RED}error:${NC} ${HOME}/.PRIDE_PPPAR_BIN dosen't exist\n"
    printf "${RED}error:${NC} PRIDE-PPPAR testing failed\n"; exit
fi

# Generate control file
wk_dir=$(pwd)   # working directory
config=ses.ppp  # configuration file
mkdir -p products
echo "## Session configure" > $config
echo "Interval = 30"       >> $config
echo "Session time    = 2016 1 1 00 00 00 86360" >> $config
echo "Rinex directory = ${wk_dir}/data/2016/001" >> $config
echo "Sp3 directory   = ${wk_dir}/products" >> $config
echo "Table directory = ${wk_dir}/../table" >> $config
cat config_templet >> $config

# Computation
pridelab_pppar.pl ses.ppp 20160101 20160101 FR

## Output
#if [ $? -eq 0 ]; then
#    printf "${BLUE}::${NC} PRIDE-PPPAR installation successfully completed!\n"
#    printf "${BLUE}::${NC} executable binaries are copy to ${HOME}/.PRIDE_PPPAR_BIN\n"
#    printf "${BLUE}::${NC} ${HOME}/.PRIDE_PPPAR_BIN added to PATH\n"
#else
#    printf "${RED}errror:${NC} PRIDE-PPPAR installation failed!\n"
#fi
