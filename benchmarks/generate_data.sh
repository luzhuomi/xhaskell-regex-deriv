#!/bin/bash
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen ${i}000000 > data/m${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "A" ${i}000000 > data/a${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "AB" ${i}000000 > data/ab${i} ; done
for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "B" ${i}000000 > data/b${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdefg" ${i}00000 > data/a_to_g${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "ABAAC" ${i}00000 > data/abaac${i} ; done


for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "ABCD" ${i}00000 > data/abcd${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdefghijklmnopqrstuvwxyz" ${i}0000 > data/a_to_z${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdefghijklm" ${i}0000 > data/a_to_m${i} ; done

for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdefgh" ${i}0000 > data/a_to_h${i} ; done


for i in 1 2 3 4 5 6 7 8 9 10; do ./gen2 "abcdef" ${i}0000 > data/a_to_f${i} ; done

for i in 10 15 20 25 30; do ./gen2 "a" ${i} > data/a_s_${i}; done

for i in 17 18 19 20 21; do ./gen3 $i > data/aabc${i} ; done

echo "X12Y" > data/x12y