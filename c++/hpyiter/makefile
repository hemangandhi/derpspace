CC = g++ --std=c++11 -O -Wall

all: mapTest

mapTest: maptest.cpp Range.h Range.cpp Map.h Map.cpp
	$(CC) maptest.cpp Range.h Range.cpp Map.h Map.cpp

clean:
	rm -rf *.out *.gch
