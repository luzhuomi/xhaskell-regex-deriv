#!/bin/bash

# case 1
rm results/DERIV1*.tsv
./Timer ./DERIV "(.)*$" results/DERIV1.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 


# case 2
rm results/DERIV2*.tsv
./Timer ./DERIV "ABCDEFGHIJKLMNOPQRSTUVWXYZ$" results/DERIV2.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 


# case 3
rm results/DERIV3*.tsv
./Timer ./DERIV "[XYZ]ABCDEFGHIJKLMNOPQRSTUVWXYZ$" results/DERIV3.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 


# case 4
rm results/DERIV4*.tsv
./Timer ./DERIV "[ -~]*ABCDEFGHIJKLMNOPQRSTUVWXYZ$" results/DERIV4.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 


# case 5
rm results/DERIV5*.tsv
./Timer ./DERIV "([0-9]{3}-|\\([0-9]{3}\\)[ ]+)([0-9]{3}-[0-9]{4})" results/DERIV5.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 

# case 6 
rm results/DERIV6*.tsv
./Timer ./DERIV "^.*([ABCDEFGHIJKLMNOPQRSTUVWXYZ][ABCDEFGHIJKLMNOPQRSTUVWXYZ]|[ABCDEFGHIJKLMNOPQRSTUVWXYZ])*(.*)$" results/DERIV6.tsv data/m1 data/m2 data/m3 data/m4 data/m5 data/m6 data/m7 data/m8 data/m9 data/m10 

# case 7
rm results/DERIV7*.tsv
./Timer ./DERIV "^(a?){10}(a){10}$" results/DERIV7.tsv data/a_s_10
./Timer ./DERIV "^(a?){15}(a){15}$" results/DERIV7.tsv data/a_s_15
./Timer ./DERIV "^(a?){20}(a){20}$" results/DERIV7.tsv data/a_s_20
./Timer ./DERIV "^(a?){25}(a){25}$" results/DERIV7.tsv data/a_s_25
./Timer ./DERIV "^(a?){30}(a){30}$" results/DERIV7.tsv data/a_s_30




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

rm results/DERIV9.tsv
./Timer ./DERIV "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/DERIV9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 

rm results/TDFA9.tsv
./Timer ./TDFA "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/TDFA9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 

rm results/re2_9.tsv
./Timer ./re2_10p "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/re2_9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g9 data/a_to_g9 data/a_to_g10 


rm results/HSPOSIX9.tsv
./Timer ./HSPOSIX "^((a)|(bcdef)|(g)|(ab)|(c)|(d)|(e)|(efg)|(fg))*$" results/HSPOSIX9.tsv data/a_to_g1 data/a_to_g2 data/a_to_g3 data/a_to_g4 data/a_to_g5 data/a_to_g6 data/a_to_g7 data/a_to_g8 data/a_to_g9 data/a_to_g10 



# case 10

rm results/DERIV10.tsv
./Timer ./DERIV "^(((A|AB)(BAA|A))(AC|C))*$" results/DERIV10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 

rm results/TDFA10.tsv
./Timer ./TDFA "^(((A|AB)(BAA|A))(AC|C))*$" results/TDFA10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 

rm results/re2_10.tsv
./Timer ./re2_5p "^(((A|AB)(BAA|A))(AC|C))*$" results/re2_10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac9 data/abaac9 data/abaac10 


rm results/HSPOSIX10.tsv
./Timer ./HSPOSIX "^(((A|AB)(BAA|A))(AC|C))*$" results/HSPOSIX10.tsv data/abaac1 data/abaac2 data/abaac3 data/abaac4 data/abaac5 data/abaac6 data/abaac7 data/abaac8 data/abaac9 data/abaac10 


# case 11

rm results/DERIV11.tsv
./Timer ./DERIV "^((A)|(B)|(C)|(D)|(AB)|(BC)|(CD)|(ABC)|(BCD)|(ABCD))*$" results/DERIV11.tsv data/abcd1 data/abcd2 data/abcd3 data/abcd4 data/abcd5 data/abcd6 data/abcd7 data/abcd8 data/abcd9 data/abcd10 

rm results/TDFA11.tsv
./Timer ./TDFA "^((A)|(B)|(C)|(D)|(AB)|(BC)|(CD)|(ABC)|(BCD)|(ABCD))*$" results/TDFA11.tsv data/abcd1 data/abcd2 data/abcd3 data/abcd4 data/abcd5 data/abcd6 data/abcd7 data/abcd8 data/abcd9 data/abcd10 

rm results/re2_11.tsv
./Timer ./re2_11p "^((A)|(B)|(C)|(D)|(AB)|(BC)|(CD)|(ABC)|(BCD)|(ABCD))*$" results/re2_11.tsv data/abcd1 data/abcd2 data/abcd3 data/abcd4 data/abcd5 data/abcd6 data/abcd7 data/abcd9 data/abcd9 data/abcd10 


rm results/HSPOSIX11.tsv
./Timer ./HSPOSIX "^((A)|(B)|(C)|(D)|(AB)|(BC)|(CD)|(ABC)|(BCD)|(ABCD))*$" results/HSPOSIX11.tsv data/abcd1 data/abcd2 data/abcd3 data/abcd4 data/abcd5 data/abcd6 data/abcd7 data/abcd8 data/abcd9 data/abcd10 


# case 12
rm results/DERIV12.tsv
./Timer ./DERIV "^(a|aa)*b$" results/DERIV12.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/TDFA12.tsv
./Timer ./TDFA "^(a|aa)*b$" results/TDFA12.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/re2_12.tsv
./Timer ./re2_1p "^(a|aa)*b$" results/re2_12.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/HSPOSIX12.tsv
./Timer ./HSPOSIX "^(a|aa)*b$" results/HSPOSIX12.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21



# case 13
rm results/DERIV13.tsv
./Timer ./DERIV "^(a|aa)*c$" results/DERIV13.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/TDFA13.tsv
./Timer ./TDFA "^(a|aa)*c$" results/TDFA13.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/re2_13.tsv
./Timer ./re2_1p "^(a|aa)*c$" results/re2_13.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


rm results/HSPOSIX13.tsv
./Timer ./HSPOSIX "^(a|aa)*c$" results/HSPOSIX13.tsv data/aabc17 data/aabc18 data/aabc19 data/aabc20 data/aabc21


# case 14
rm results/DERIV14.tsv
./Timer ./DERIV "^.*X(.?){1,2}Y.*$" results/DERIV14.tsv data/x12y
./Timer ./DERIV "^.*X(.?){1,3}Y.*$" results/DERIV14.tsv data/x12y
./Timer ./DERIV "^.*X(.?){1,4}Y.*$" results/DERIV14.tsv data/x12y
./Timer ./DERIV "^.*X(.?){1,5}Y.*$" results/DERIV14.tsv data/x12y
./Timer ./DERIV "^.*X(.?){1,6}Y.*$" results/DERIV14.tsv data/x12y

rm results/TDFA14.tsv
./Timer ./TDFA "^.*X(.?){1,2}Y.*$" results/TDFA14.tsv data/x12y
./Timer ./TDFA "^.*X(.?){1,3}Y.*$" results/TDFA14.tsv data/x12y
./Timer ./TDFA "^.*X(.?){1,4}Y.*$" results/TDFA14.tsv data/x12y
./Timer ./TDFA "^.*X(.?){1,5}Y.*$" results/TDFA14.tsv data/x12y
./Timer ./TDFA "^.*X(.?){1,6}Y.*$" results/TDFA14.tsv data/x12y

rm results/re2_14.tsv
./Timer ./re2_1p "^.*X(.?){1,2}Y.*$" results/re2_14.tsv data/x12y
./Timer ./re2_1p "^.*X(.?){1,3}Y.*$" results/re2_14.tsv data/x12y
./Timer ./re2_1p "^.*X(.?){1,4}Y.*$" results/re2_14.tsv data/x12y
./Timer ./re2_1p "^.*X(.?){1,5}Y.*$" results/re2_14.tsv data/x12y
./Timer ./re2_1p "^.*X(.?){1,6}Y.*$" results/re2_14.tsv data/x12y

rm results/HSPOSIX14.tsv
./Timer ./HSPOSIX "^.*X(.?){1,2}Y.*$" results/HSPOSIX14.tsv data/x12y
./Timer ./HSPOSIX "^.*X(.?){1,3}Y.*$" results/HSPOSIX14.tsv data/x12y
./Timer ./HSPOSIX "^.*X(.?){1,4}Y.*$" results/HSPOSIX14.tsv data/x12y
./Timer ./HSPOSIX "^.*X(.?){1,5}Y.*$" results/HSPOSIX14.tsv data/x12y
./Timer ./HSPOSIX "^.*X(.?){1,6}Y.*$" results/HSPOSIX14.tsv data/x12y



# case 15
rm results/DERIV15.tsv
./Timer ./DERIV "^((((((((((((((((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))(n))(o))(p))(q))(r))(s))(t))(u))(v))(w))(x))(y))(z))*$" results/DERIV15.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/TDFA15.tsv
./Timer ./TDFA "^((((((((((((((((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))(n))(o))(p))(q))(r))(s))(t))(u))(v))(w))(x))(y))(z))*$" results/TDFA15.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/HSPOSIX15.tsv
./Timer ./HSPOSIX "^((((((((((((((((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))(n))(o))(p))(q))(r))(s))(t))(u))(v))(w))(x))(y))(z))*$" results/HSPOSIX15.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/re2_15.tsv
./Timer ./re2_1p "^((((((((((((((((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))(n))(o))(p))(q))(r))(s))(t))(u))(v))(w))(x))(y))(z))*$" results/re2_15.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10



# case 16
rm results/DERIV16.tsv
./Timer ./DERIV "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)((l)((m)((n)((o)((p)((q)((r)((s)((t)((u)((v)((w)((x)(y)))))))))))))))))))))))))(z))*$" results/DERIV16.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/TDFA16.tsv
./Timer ./TDFA "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)((l)((m)((n)((o)((p)((q)((r)((s)((t)((u)((v)((w)((x)(y)))))))))))))))))))))))))(z))*$" results/TDFA16.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/HSPOSIX16.tsv
./Timer ./HSPOSIX "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)((l)((m)((n)((o)((p)((q)((r)((s)((t)((u)((v)((w)((x)(y)))))))))))))))))))))))))(z))*$" results/HSPOSIX16.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


rm results/re2_16.tsv
./Timer ./re2_1p  "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)((l)((m)((n)((o)((p)((q)((r)((s)((t)((u)((v)((w)((x)(y)))))))))))))))))))))))))(z))*$" results/re2_16.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10


# case 17
#rm results/DERIV17_1.tsv
#./Timer ./DERIV  "^((((((((((((((((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))(n))(o))(p))(q))(r))(s))(t))(u))(v))(w))(x))(y))(z))*$" results/DERIV17_1.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10
ln -snvf results/DERIV15.tsv results/DERIV17_1.tsv


rm results/DERIV17_05.tsv
./Timer ./DERIV "^(((((((((((((a)(b))(c))(d))(e))(f))(g))(h))(i))(j))(k))(l))(m))*$" results/DERIV17_05.tsv data/a_to_m1 data/a_to_m2 data/a_to_m3 data/a_to_m4 data/a_to_m5 data/a_to_m6 data/a_to_m7 data/a_to_m8 data/a_to_m9 data/a_to_m10

rm results/DERIV17_025.tsv
./Timer ./DERIV "^(((((((a))(b))(c))(d))(e))(f))*$" results/DERIV17_025.tsv data/a_to_f1 data/a_to_f2 data/a_to_f3 data/a_to_f4 data/a_to_f5 data/a_to_f6 data/a_to_f7 data/a_to_f8 data/a_to_f9 data/a_to_f10



# case 18
#rm results/DERIV18_1.tsv
#./Timer ./DERIV  "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)((l)((m)((n)((o)((p)((q)((r)((s)((t)((u)((v)((w)((x)(y)))))))))))))))))))))))))(z))*$" results/DERIV18_1.tsv data/a_to_z1 data/a_to_z2 data/a_to_z3 data/a_to_z4 data/a_to_z5 data/a_to_z6 data/a_to_z7 data/a_to_z8 data/a_to_z9 data/a_to_z10

ln -snvf results/DERIV16.tsv results/DERIV18_1.tsv


rm results/DERIV18_05.tsv
./Timer ./DERIV "^(((a)((b)((c)((d)((e)((f)((g)((h)((i)((j)((k)(l))))))))))))(m))*$" results/DERIV18_05.tsv data/a_to_m1 data/a_to_m2 data/a_to_m3 data/a_to_m4 data/a_to_m5 data/a_to_m6 data/a_to_m7 data/a_to_m8 data/a_to_m9 data/a_to_m10

rm results/DERIV18_025.tsv
./Timer ./DERIV "^(((a)((b)((c)((d)(e)))))(f))*$" results/DERIV18_025.tsv data/a_to_f1 data/a_to_f2 data/a_to_f3 data/a_to_f4 data/a_to_f5 data/a_to_f6 data/a_to_f7 data/a_to_f8 data/a_to_f9 data/a_to_f10






# case 19
rm results/DERIV19.tsv
./Timer ./DERIV "^((((((((a)(b))(c))(d))(e))(f))(g))(h))*$" results/DERIV19.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/TDFA19.tsv
./Timer ./TDFA "^((((((((a)(b))(c))(d))(e))(f))(g))(h))*$" results/TDFA19.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/HSPOSIX19.tsv
./Timer ./HSPOSIX "^((((((((a)(b))(c))(d))(e))(f))(g))(h))*$" results/HSPOSIX19.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/re2_19.tsv
./Timer ./re2_1p "^((((((((a)(b))(c))(d))(e))(f))(g))(h))*$" results/re2_19.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10



# case 20
rm results/DERIV20.tsv
./Timer ./DERIV "^(((a)((b)((c)((d)((e)((f)(g)))))))(h))*$" results/DERIV20.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/TDFA20.tsv
./Timer ./TDFA "^(((a)((b)((c)((d)((e)((f)(g)))))))(h))*$" results/TDFA20.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/HSPOSIX20.tsv
./Timer ./HSPOSIX "^(((a)((b)((c)((d)((e)((f)(g)))))))(h))*$" results/HSPOSIX20.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10


rm results/re2_20.tsv
./Timer ./re2_1p "^(((a)((b)((c)((d)((e)((f)(g)))))))(h))*$" results/re2_20.tsv data/a_to_h1 data/a_to_h2 data/a_to_h3 data/a_to_h4 data/a_to_h5 data/a_to_h6 data/a_to_h7 data/a_to_h8 data/a_to_h9 data/a_to_h10





# case 22
rm results/DERIV22.tsv
echo "" > data/empty
./Timer ./DERIV "^((.?){1,1}Y)*X.*$" results/DERIV22.tsv  data/empty
./Timer ./DERIV "^((.?){1,2}Y)*X.*$" results/DERIV22.tsv  data/empty
./Timer ./DERIV "^((.?){1,3}Y)*X.*$" results/DERIV22.tsv  data/empty
./Timer ./DERIV "^((.?){1,4}Y)*X.*$" results/DERIV22.tsv  data/empty
./Timer ./DERIV "^((.?){1,5}Y)*X.*$" results/DERIV22.tsv  data/empty


rm results/TDFA22.tsv
echo "" > data/empty
./Timer ./TDFA "^((.?){1,1}Y)*X.*$" results/TDFA22.tsv  data/empty
./Timer ./TDFA "^((.?){1,2}Y)*X.*$" results/TDFA22.tsv  data/empty
./Timer ./TDFA "^((.?){1,3}Y)*X.*$" results/TDFA22.tsv  data/empty
./Timer ./TDFA "^((.?){1,4}Y)*X.*$" results/TDFA22.tsv  data/empty
./Timer ./TDFA "^((.?){1,5}Y)*X.*$" results/TDFA22.tsv  data/empty



rm results/HSPOSIX22.tsv
echo "" > data/empty
./Timer ./HSPOSIX "^((.?){1,1}Y)*X.*$" results/HSPOSIX22.tsv  data/empty
./Timer ./HSPOSIX "^((.?){1,2}Y)*X.*$" results/HSPOSIX22.tsv  data/empty
./Timer ./HSPOSIX "^((.?){1,3}Y)*X.*$" results/HSPOSIX22.tsv  data/empty
./Timer ./HSPOSIX "^((.?){1,4}Y)*X.*$" results/HSPOSIX22.tsv  data/empty
./Timer ./HSPOSIX "^((.?){1,5}Y)*X.*$" results/HSPOSIX22.tsv  data/empty


rm results/re2_22.tsv
echo "" > data/empty
./Timer ./re2_1p "^((.?){1,1}Y)*X.*$" results/re2_22.tsv  data/empty
./Timer ./re2_1p "^((.?){1,2}Y)*X.*$" results/re2_22.tsv  data/empty
./Timer ./re2_1p "^((.?){1,3}Y)*X.*$" results/re2_22.tsv  data/empty
./Timer ./re2_1p "^((.?){1,4}Y)*X.*$" results/re2_22.tsv  data/empty
./Timer ./re2_1p "^((.?){1,5}Y)*X.*$" results/re2_22.tsv  data/empty




