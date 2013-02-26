#!/bin/bash


# case 8
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8ab.tsv data/ab1 data/ab2 data/ab3 data/ab4 data/ab5 data/ab6 data/ab7 data/ab8 data/ab9 data/ab10 
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8a.tsv data/b1 data/b2 data/b3 data/b4 data/b5 data/b6 data/b7 data/b8 data/b9 data/b10 


./Timer ./TDFA "^((A)|(AB)|(B))*$" results/TDFA8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
