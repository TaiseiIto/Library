SOURCES = $(wildcard *.hs)

all: test

clear:
	rm test

rebuild: clear
	make

test: $(SOURCES)
	ghc test.hs

