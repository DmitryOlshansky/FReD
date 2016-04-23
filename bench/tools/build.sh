#!/bin/bash
if [ -z "$DMD" ] ; then
    DMD=dmd
fi
if [ $# -ne 1 ] ; then
	echo "Usage: ./build.sh <filename.d>"
	exit 1
fi

$DMD -O -release -inline -version=backtracking  $1 -ofd_bt
$DMD -O -release -inline -version=thompson  $1 -ofd_th
IDX=0
while read -r n; do
    if [ -n "$n" ] && ! (echo -n "$n" | grep -e "^\s*#"); then 
        echo -n "$n" > ct_pattern
        echo "$IDX:$n"
        $DMD -O -release -inline -version=ct_regex -J. $1 -ofct_r$IDX
        let IDX=IDX+1
    fi
done < patterns.txt
