GHC = ghc
CXXFLAGS = -std=c++11 -O3
GHCFLAGS = -O3

all: fastacpp fastahs

fastacpp: fasta.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

fastahs: Fasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^
