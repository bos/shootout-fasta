GHC = ghc
CXXFLAGS = -std=c++11 -O3
GHCFLAGS = -O3 -fllvm -Wall

all: fastacpp fastahs fastac

cpp: fastacpp

c: fastac

fastacpp: fasta.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

fastac: fasta.c
	$(CC) $(CFLAGS) -o $@ $^

hs: fastahs

fastahs: Fasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

clean:
	-rm -f fastahs fastacpp
