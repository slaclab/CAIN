#!/bin/bash
# extract_stat.sh  —  usage: ./extract_stat.sh log_HiggsIP_v08770.txt 0 500 2000 4000
log="$1"; shift
steps="$*"

awk -v steps="$steps" '
BEGIN {
    n = split(steps, a, " ")
    maxw = 0
    for (i = 1; i <= n; i++) { want[a[i]+0] = 1; if (a[i]+0 > maxw) maxw = a[i]+0 }
    cur = -1; prev = -1
}
/-th time step/ {
    s = $0
    sub(/^ *\+\+\+ */, "", s)      # strip leading " +++ "
    sub(/\..*$/, "", s)            # keep digits before the "."
    cur = s + 0
    if (cur > maxw) exit           # early exit: past last wanted step
    inblk = (cur in want)
    if (inblk && cur != prev) print "\n########## it=" cur " ##########"
    prev = cur
}
inblk { print }
' "$log"
