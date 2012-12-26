{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-  The Computer Language Benchmarks Game

    http://shootout.alioth.debian.org/

    contributed by Branimir Maksimovic
-}

module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString as B
import Control.Monad (when, void)
import Data.Array.Base
import Data.Word (Word8)

import Foreign.Ptr
import Foreign.Storable (peek, poke)
import Foreign.C.Types

type C = (UArray Int Word8,UArray Int Float)

foreign import ccall unsafe "stdio.h"
     puts  :: Ptr a -> IO ()

main :: IO ()
main = do
    n <- getArgs >>= readIO.head

    B.unsafeUseAsCString alu $ \ptr -> void $
      make ">ONE Homo sapiens alu" (n*2) (Main.repeat ptr (B.length alu)) 0
    make ">TWO IUB ambiguity codes" (n*3) (genRandom iub) 42 >>=
      void . make ">THREE Homo sapiens frequency" (n*5) (genRandom homosapiens)

make :: B.ByteString -> Int -> (Int -> W) -> Int -> IO Int
{-# INLINE make #-}
make name n f seed0 = do
    B.unsafeUseAsCString name puts
    let line = B.replicate 61 0
    B.unsafeUseAsCString line $ \ptr -> do
      let make' !n !i !seed
	      | n > 0 = do
		  let W newseed c = f seed
		  poke (ptr `plusPtr` i) c
		  if i+1 >= 60
		      then puts ptr >> make' (n-1) 0 newseed
		      else make' (n-1) (i+1) newseed
	      | otherwise = do
		  when (i > 0) $ poke (ptr `plusPtr` i) (0::CChar) >> puts ptr
		  return seed
      make' n 0 seed0

repeat :: Ptr CChar -> Int -> Int -> W
repeat xs n i = B.inlinePerformIO $ W i' `fmap` peek (xs `plusPtr` i)
    where i' | i1 >= n   = 0
             | otherwise = i1
          i1 = i + 1

data W = W {-# UNPACK #-} !Int {-# UNPACK #-} !Word8

genRandom :: C -> Int -> W
genRandom (!a,!b) seed = find 0
  where find i
            | b `unsafeAt` i >= rnd = W newseed (a `unsafeAt` i)
            | otherwise = find (i+1)
        D newseed rnd = genRand seed

data D = D {-# UNPACK #-} !Int {-# UNPACK #-} !Float

genRand :: Int -> D
genRand seed = D newseed newran
  where
    newseed = (seed * ia + ic) `rem` im
    newran  =  fromIntegral newseed * rimd
    rimd      = 1.0 / fromIntegral im
    im, ia, ic :: Int
    im  = 139968
    ia  = 3877
    ic  = 29573

alu :: B.ByteString
alu =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
    \GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
    \CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
    \ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
    \GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
    \AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
    \AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

mkCum :: [(Char,Float)] -> C
mkCum lst = (listArray (0, length lst - 1) (map fst ab),
      	     listArray (0, length lst - 1) (map snd ab))
  where ab = map (\(c,p) -> ((fromIntegral.fromEnum) c,p)) .
             scanl1 (\(_,p) (c',p') -> (c', p+p')) $ lst

homosapiens, iub :: C

iub = mkCum [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens = mkCum [('a',0.3029549426680),('c',0.1979883004921)
                ,('g',0.1975473066391),('t',0.3015094502008)]
