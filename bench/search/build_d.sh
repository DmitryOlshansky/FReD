#!/bin/bash
if [ -z "$DMD" ] ; then
    DMD=dmd
fi
$DMD -O -release -inline -version=backtracking  d_r.d -ofd_bt
$DMD -O -release -inline -version=thompson  d_r.d -ofd_th
IDX=0
while read -r n; do
    if [ -n "$n" ] && ! (echo -n "$n" | grep -e "^\s*#"); then 
        echo -n "$n" > ct_pattern
        echo "$IDX:$n"
        $DMD -O -release -inline -version=ct_regex -J. d_r.d -ofct_r$IDX
        let IDX=IDX+1
    fi
done < patterns.txt
