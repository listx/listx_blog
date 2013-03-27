all:
	ghc -O2 -Wall -fno-spec-constr-count -threaded --make blog
	./blog rebuild
	./sync.sh

nosync:
	ghc -O2 -Wall -fno-spec-constr-count -threaded --make blog
	./blog rebuild

prod:
	ghc -O2 -Wall -Werror -fno-spec-constr-count -threaded --make blog

prof:
	ghc -rtsopts -prof -auto-all -O2 -v -Wall -Werror --make blog
	./blog +RTS -p

srclist:
	$(shell find -type f -regex ".*\.hs" > srclist)

clean:
	$(RM) blog
	find -type f -iregex ".+\.\(o\|hi\|hp\|mix\|ps\|tix\)" -exec rm -v {} \;
