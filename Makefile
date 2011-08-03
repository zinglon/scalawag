# build with SDL C wrapper for mac osx (see mainc.c)
main: *.hs
	ghc --make Main.hs -o $@

clean:
	rm -f *.hi *.o main

.PHONY: clean
