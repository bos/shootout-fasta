This is a Haskell implementation of the [fasta
benchmark](http://benchmarksgame.alioth.debian.org/u32/performance.php?test=fasta)
from the [Computer Language Benchmark
Game](http://benchmarksgame.alioth.debian.org/).

I've structured the repository's commits so you can see various
performance experiments.

The [Haskell code that I originally started
with](https://github.com/bos/shootout-fasta/blob/e3c995cbe8d830522c15d9ddcbc54c5217d3c1bf/Fasta.hs)
was written by Branimir Maksimovic. His program was an almost direct
port of his C++ implementation, but was much slower.

I very quickly [improved his
code](https://github.com/bos/shootout-fasta/blob/11c72b8bed87ae088181ea56fb95d51dfb605669/Fasta.hs),
and hit the limit of what was achievable with that approach.

My [subsequent
rewrite](https://github.com/bos/shootout-fasta/blob/master/Fasta.hs)
bears just about no resemblance to his original code.
