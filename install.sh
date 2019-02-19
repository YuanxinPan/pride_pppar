#!/bin/bash
# Install PRIDE-PPPAR

RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check compiler
gfortran --version > /dev/null 2>&1
if [ $? -ne 0 ]; then
    printf "${RED}error:${NC} no compiler: gfortran\n"
    printf "${RED}error:${NC} PRIDE-PPPAR installation failed\n"; exit
fi
make --version > /dev/null 2>&1
if [ $? -ne 0 ]; then
    printf "${RED}error:${NC} GNU make not found\n"
    printf "${RED}error:${NC} PRIDE-PPPAR installation failed\n"; exit
fi

# Compilation & Installation
install_dir=${HOME}/.PRIDE_PPPAR_BIN
rm -rf "$install_dir"
cd src && make && make install \
    && cd .. \
    && mkdir -p $install_dir \
    && cp -f ./bin/* $install_dir \
    && cp -f ./scripts/pride_pppar.sh $install_dir/pride_pppar \
    && cp -f ./scripts/rtk2xyz.sh ./scripts/leap.sh $install_dir \
    && echo "export PATH=$install_dir:\$PATH" >> ${HOME}/.bashrc

# Output
if [ $? -eq 0 ]; then
    printf "${BLUE}::${NC} PRIDE-PPPAR installation successfully completed!\n"
    printf "${BLUE}::${NC} executable binaries are copy to $install_dir\n"
    printf "${BLUE}::${NC} $install_dir added to PATH\n"
else
    printf "${RED}errror:${NC} PRIDE-PPPAR installation failed!\n"
fi
