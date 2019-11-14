sync: rebuild
	./sync.sh

rebuild:
	stack build
	stack exec -- blog rebuild

.PHONY: sync nosync
