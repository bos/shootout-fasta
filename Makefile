GHC = ghc

all: fastacpp fastahs

fastacpp: fasta.cpp
	$(CXX) -O3 -o $@ $^

fastahs: Fasta.hs
	$(GHC) -O3 --make -o $@ $^
