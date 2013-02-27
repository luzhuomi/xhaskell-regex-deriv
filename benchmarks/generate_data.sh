#!/bin/bash
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen ${i}000000 > data/m${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "A" ${i}000000 > data/a${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "AB" ${i}000000 > data/ab${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "B" ${i}000000 > data/b${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdefg" ${i}00000 > data/a_to_g${i} ; done


for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "ABAAC" ${i}00000 > data/abaac${i} ; done
