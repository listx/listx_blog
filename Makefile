OBJ = dist/build/blog/blog-tmp/Main.o dist/build/base/base-tmp/Main.o
all: $(OBJ)
	cabal build --ghc-options "-O2 -Wall" blog
	cp dist/build/blog/blog .
	./blog rebuild
	./sync.sh

nosync: $(OBJ)
	cabal build --ghc-options "-O2 -Wall" blog
	cp dist/build/blog/blog .
	./blog rebuild

srclist:
	$(shell find -type f -regex ".*\.hs" > srclist)

clean:
	$(RM) blog
	find -type f -iregex ".+\.\(o\|hi\|hp\|mix\|ps\|tix\)" -exec rm -v {} \;
