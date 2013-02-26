all:
	g++ -I/usr/local/include/ -L/usr/local/lib/ re2.cpp -lre2 -pthread -o re2
	ghc --make -O2 -rtsopts -fforce-recomp DERIV.lhs
	ghc --make -O2 -rtsopts -fforce-recomp TDFA.lhs
	ghc --make -O2 -rtsopts -fforce-recomp HSPOSIX.lhs
	ghc --make -O2 gen.hs
	ghc --make -O2 gen2.hs
	ghc --make -O2 Timer.hs
clean:
	rm *.o *.hi
	rm re2 DERIV TDFA HSPOSIX

