{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-  The Computer Language Benchmarks Game

    http://shootout.alioth.debian.org/

    contributed by Bryan O'Sullivan
-}

module Main (main) where

import Control.Monad
import System.Environment
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy.Builder as B
import Data.Monoid
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    n <- getArgs >>= readIO.head

    writeAlu ">ONE Homo sapiens alu" (L.take (fromIntegral n*2) (L.cycle alu))
    make ">TWO IUB ambiguity codes" (n*3) iub 42 >>=
      void . make ">THREE Homo sapiens frequency" (n*5) homosapiens

writeAlu :: B.ByteString -> L.ByteString -> IO ()
writeAlu name s0 = B.putStrLn name >> go s0
 where go s
         | L.null t = L.putStrLn h >> return ()
         | otherwise = L.putStrLn h >> go t
         where (h,t) = L.splitAt 60 s

make :: B.ByteString -> Int -> [(Char, Float)] -> Int -> IO Int
make name n0 tbl seed0 = do
  B.putStrLn name
  let modulus = 139968
      convert = scanl1 (\(_,p) (c,q) -> (c,p+q))
      fill ((c,p):cps) !j =
	let !k = min modulus (floor (fromIntegral modulus * p + 1))
	in B.replicate (fromIntegral (k - j)) c : fill cps k
      fill _ _ = []
  let lookupTable = B.concat $ fill (convert tbl) 0
      rnds = V.iterateN (n0+1) (\s -> (s * 3877 + 29573) `rem` modulus) seed0
      make' n !i
	  | n <= n0 =
	      let !w = lookupTable `B.unsafeIndex` (rnds `V.unsafeIndex` n)
	          b = B.word8 w
	      in if i == 60
                 then B.word8 10 <> b <> make' (n+1) 1
		 else b <> make' (n+1) (i+1)
	  | otherwise = mempty
  L.putStrLn . B.toLazyByteString $ make' 1 (0::Int)
  return $! V.last rnds

alu :: L.ByteString
alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGG\
    \TCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGG\
    \CGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGC\
    \GGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iub, homosapiens :: [(Char, Float)]

iub = [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
      ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
      ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = [('a',0.3029549426680),('c',0.1979883004921)
              ,('g',0.1975473066391),('t',0.3015094502008)]
