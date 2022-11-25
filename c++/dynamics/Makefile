SOURCES = $(wildcard *.cpp)
OBJECTS = $(SOURCES:.cpp=.o)

all: test

test: $(OBJECTS)
	g++ $^ -o $@

%.o: %.c
	g++ $^ -o $@

