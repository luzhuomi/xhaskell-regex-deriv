all:
	g++ -I/usr/local/include/ -L/usr/local/lib/ re2_4p.cpp -lre2 -pthread -o re2_4p
	g++ -I/usr/local/include/ -L/usr/local/lib/ re2_5p.cpp -lre2 -pthread -o re2_5p
	g++ -I/usr/local/include/ -L/usr/local/lib/ re2_10p.cpp -lre2 -pthread -o re2_10p
	g++ -I/usr/local/include/ -L/usr/local/lib/ re2_11p.cpp -lre2 -pthread -o re2_11p
	ghc --make -O2 -rtsopts -fforce-recomp DERIV.lhs
	ghc --make -O2 -rtsopts -fforce-recomp TDFA.lhs
	ghc --make -O2 -rtsopts -fforce-recomp HSPOSIX.lhs
	ghc --make -O2 gen.hs
	ghc --make -O2 gen2.hs
	ghc --make -O2 Timer.hs
clean:
	rm *.o *.hi
	rm re2_4p re2_5p re2_10p re2_11p DERIV TDFA HSPOSIX

