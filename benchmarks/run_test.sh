#!/bin/bash


# case 8
rm results/DERIV8*.tsv
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8ab.tsv data/ab1 data/ab2 data/ab3 data/ab4 data/ab5 data/ab6 data/ab7 data/ab8 data/ab9 data/ab10 
./Timer ./DERIV "^((A)|(AB)|(B))*$" results/DERIV8b.tsv data/b1 data/b2 data/b3 data/b4 data/b5 data/b6 data/b7 data/b8 data/b9 data/b10 

rm results/TDFA8*.tsv
./Timer ./TDFA "^((A)|(AB)|(B))*$" results/TDFA8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
./Timer ./TDFA "^((A)|(AB)|(B))*$" results/TDFA8ab.tsv data/ab1 data/ab2 data/ab3 data/ab4 data/ab5 data/ab6 data/ab7 data/ab8 data/ab9 data/ab10
./Timer ./TDFA "^((A)|(AB)|(B))*$" results/TDFA8b.tsv data/b1 data/b2 data/b3 data/b4 data/b5 data/b6 data/b7 data/b8 data/b9 data/b10

rm results/re2_8*.tsv

./Timer ./re2_4p "^((A)|(AB)|(B))*$" results/re2_8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
./Timer ./re2_4p "^((A)|(AB)|(B))*$" results/re2_8ab.tsv data/ab1 data/ab2 data/ab3 data/ab4 data/ab5 data/ab6 data/ab7 data/ab8 data/ab9 data/ab10
./Timer ./re2_4p "^((A)|(AB)|(B))*$" results/re2_8b.tsv data/b1 data/b2 data/b3 data/b4 data/b5 data/b6 data/b7 data/b8 data/b9 data/b10


rm results/HSPOSIX8*.tsv

./Timer ./HSPOSIX "^((A)|(AB)|(B))*$" results/HSPOSIX8a.tsv data/a1 data/a2 data/a3 data/a4 data/a5 data/a6 data/a7 data/a8 data/a9 data/a10 
./Timer ./HSPOSIX "^((A)|(AB)|(B))*$" results/HSPOSIX8ab.tsv data/ab1 data/ab2 data/ab3 data/ab4 data/ab5 data/ab6 data/ab7 data/ab8 data/ab9 data/ab10
./Timer ./HSPOSIX "^((A)|(AB)|(B))*$" results/HSPOSIX8b.tsv data/b1 data/b2 data/b3 data/b4 data/b5 data/b6 data/b7 data/b8 data/b9 data/b10


# case 9

rm results/DERIV9*.tsv
./Timer ./DERIV "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/DERIV9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 

rm results/TDFA9*.tsv
./Timer ./TDFA "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/TDFA9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 

rm results/re2_9*.tsv
./Timer ./re2_10p "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/re2_9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g9 data/a_to_g9 data/a_to_g10 


rm results/HSPOSIX9*.tsv
./Timer ./HSPOSIX "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/HSPOSIX9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 



# case 10

rm results/DERIV10.tsv
./Timer ./DERIV "^((A|AB)(BAA|A)(AC|C))*$" results/DERIV10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 

rm results/TDFA10.tsv
./Timer ./TDFA "^((A|AB)(BAA|A)(AC|C))*$" results/TDFA10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 

rm results/re2_10.tsv
./Timer ./re2_4p "^((A|AB)(BAA|A)(AC|C))*$" results/re2_10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac9 data/abaac9 data/abaac10 


rm results/HSPOSIX10.tsv
./Timer ./HSPOSIX "^((A|AB)(BAA|A)(AC|C))*$" results/HSPOSIX10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 
