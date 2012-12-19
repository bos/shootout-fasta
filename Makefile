GHC = ghc
CXXFLAGS = -std=c++11 -O3
GHCFLAGS = -O3

all: fastacpp fastahs

cpp: fastacpp

fastacpp: fasta.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

hs: fastahs

fastahs: Fasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

clean:
	-rm -f fastahs fastacpp
