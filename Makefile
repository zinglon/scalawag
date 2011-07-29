# build with SDL C wrapper for mac osx (see mainc.c)
main: Main.hs
	ghc --make Main.hs -o $@

clean:
	rm -f *.hi *.o main

.PHONY: clean
