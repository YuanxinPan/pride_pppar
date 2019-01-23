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
    printf "${RED}error:${NC} PRIDE-PPPAR installation failed\n"
    exit
fi

# Compilation & Installation
cd src && make && make install \
    && cd .. \
    && cp -rf ./bin/ ${HOME}/.PRIDE_PPPAR_BIN \
    && cp -f ./scripts/pridelab_pppar.pl ${HOME}/.PRIDE_PPPAR_BIN \
    && echo "export PATH=${HOME}/.PRIDE_PPPAR_BIN:$PATH" >> ${HOME}/.bashrc

# Output
if [ $? -eq 0 ]; then
    printf "${BLUE}::${NC} PRIDE-PPPAR installation successfully completed!\n"
    printf "${BLUE}::${NC} executable binaries are copy to ${HOME}/.PRIDE_PPPAR_BIN\n"
    printf "${BLUE}::${NC} ${HOME}/.PRIDE_PPPAR_BIN added to PATH\n"
else
    printf "${RED}errror:${NC} PRIDE-PPPAR installation failed!\n"
fi
