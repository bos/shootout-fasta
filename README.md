This is a Haskell implementation of the [fasta
benchmark](http://benchmarksgame.alioth.debian.org/u32/performance.php?test=fasta)
from the [Computer Language Benchmark
Game](http://benchmarksgame.alioth.debian.org/).

I've structured the repository's commits so you can see various
performance experiments.

The Haskell code that I originally started with was written by
Branimir Maksimovic. His program was an almost direct port of his C++
implementation, and was very slow.

I very quickly improved his code, and hit the limit of what was
achievable with that approach.

My subsequent rewrite bears just about no resemblance to his original
code.
