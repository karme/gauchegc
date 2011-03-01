#!/bin/sh

for t in 0 1 2; do
    for m in 2 4 8 16 32 64 128 256; do
        for n in 10 20 40 80 160; do
            gosh bench.scm $t $n $m < /dev/zero >> test$t.dat
        done
        echo >> test$t.dat
        echo >> test$t.dat
    done
done


