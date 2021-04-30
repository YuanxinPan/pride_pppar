#!/bin/bash

[ $# -ne 1 ] && echo "usage: enucov pos_file" && exit 1

[ ! -f "$1" ] && echo "no such file: $1" && exit 1

for site in `awk '{print $1}' $1`
do
    echo $site
    grep $site "$1" > tmp_$site
    enucov.e tmp_$site && cat enucov.out >> cov
    rm -f tmp_$site enucov.out
done
