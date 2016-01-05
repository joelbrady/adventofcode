#!/bin/bash


prefix=bgvyzdsv

for n in `/usr/bin/seq 2000000`; do
    s="$prefix$n"
    digest=`echo -n $s | md5`
    if echo $digest | grep '$00000'; then
        echo $s
        exit 0
    fi
done
