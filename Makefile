main: main.hs il.hs ill.hs sequentc.hs
	ghc --make main.hs

clean:
	rm -f *.o *.hi main *~
.PHONY: clean
