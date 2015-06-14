#!/bin/bash
# first argument - file
if [ $# -ne 0 ] ; then
    ARG=$1
else
    ARG=howto
fi
echo "Running tests for $ARG" 1>&2

RUNS=3 # pick best of RUNS runs

# args - program pattern file, stdout - best of RUNS in seconds
bench(){
    PERLISM='$min = 0; while(<>){ if(/elapsed ([0-9.e-]+)/){ if($min < $1){ $min = $1; } } } print $min;'
    let X=0
    while [ $X -lt $RUNS ] ; do
       ./$1 "$2" "$3"
       let X=X+1
    done | perl -e "$PERLISM"
}

list_exec(){
    find -type f | while read f; do
        if [ -x $f ] && !(echo -n "$f" | grep -q -E '.*\.sh|ct_r.*' ); then
            echo "$f"
        fi
    done 
}

BINARIES=$(list_exec)
RUNS=3
IDX=0
echo -n "n,pattern,\"CT-regex\"," 
for b in $BINARIES; do
    echo -n "$b," | sed "s|./||"
done
echo ""
while read -r n; do
    if [ -n "$n" ] && ! (echo -n "$n" | grep -q -E "^\s*#"); then
        echo -n "$IDX,\"$n\","
        for impl in "ct_r$IDX" $BINARIES; do
            bench "$impl" "$n" "$ARG"
            echo -n ","
        done
        let IDX=IDX+1
        echo ""
    fi
done < patterns.txt
