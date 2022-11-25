SOURCES = $(wildcard *.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

all: test

clear:
	rm test *.o

rebuild: clear
	make

test: $(OBJECTS)
	g++ $^ -o $@

%.o: %.c
	g++ $^ -o $@ -Wall -Wextra

