main: il.hs ill.hs main.hs sequentc.hs
	ghc --make main.hs

clean:
	rm -f *.o *.hi main *~
.PHONY: clean
