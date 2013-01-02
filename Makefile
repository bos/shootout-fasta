GHC = ghc
CXXFLAGS = -std=c++11 -O3
GHCFLAGS = -O3 -Wall -rtsopts
CFLAGS = -O3

all: hs cpp c

cpp: fastacpp

c: fastac

fastacpp: fasta.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

fastac: fasta.c
	$(CC) $(CFLAGS) -o $@ $^

hs: fastahs oldfastahs knukehs oldknukehs

fastahs: Fasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

oldfastahs: OldFasta.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

knukehs: KNuke.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

oldknukehs: OldKNuke.hs
	$(GHC) $(GHCFLAGS) --make -o $@ $^

clean:
	-rm -f fastahs fastacpp oldfastahs fastac
