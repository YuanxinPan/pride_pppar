#!/bin/bash
# Install PANDA-PPP

RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

cd src && make && make install \
    && cd .. \
    && cp -rf ./bin/ ${HOME}/.PANDA_BIN \
    && echo "export PATH=${HOME}/.PANDA_BIN:$PATH" >> ${HOME}/.bashrc \
    && source ${HOME}/.bashrc

if [ $? -eq 0 ]; then
    printf "${BLUE}::${NC} PANDA installation successfully completed!\n"
    printf "${BLUE}::${NC} executable binaries are copy to ${HOME}/.PANDA_BIN\n"
    printf "${BLUE}::${NC} ${HOME}/.PANDA_BIN added to PATH\n"
else
    printf "${RED}errror:${NC} PANDA installation failed!\n"
fi
