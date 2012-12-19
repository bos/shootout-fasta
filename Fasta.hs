{-# LANGUAGE BangPatterns #-}
{-  The Computer Language Benchmarks Game

    http://shootout.alioth.debian.org/

    contributed by Branimir Maksimovic
-}

import System.Environment
import System.IO.Unsafe

import Data.IORef
import Data.Array.Unboxed
import Data.Array.Storable
import Data.Array.Base
import Data.Word

import Foreign.Ptr
import Foreign.C.Types

type A = UArray Int Word8
type B = StorableArray Int Word8
type C = (UArray Int Word8,UArray Int Double)

foreign import ccall unsafe "stdio.h"
     puts  :: Ptr a -> IO ()
foreign import ccall unsafe "string.h"
     strlen :: Ptr a -> IO CInt

main :: IO ()
main = do
    n <- getArgs >>= readIO.head

    let !a = (listArray (0,(length alu)-1)
             $ map (fromIntegral. fromEnum) alu:: A)
    make "ONE" "Homo sapiens alu" (n*2) $ Main.repeat a (length alu)
    make "TWO"  "IUB ambiguity codes" (n*3) $ random iub
    make "THREE" "Homo sapiens frequency" (n*5) $ random homosapiens

make :: String -> String -> Int -> IO Word8 -> IO ()
{-# INLINE make #-}
make id desc n f = do
    let lst = ">" ++ id ++ " " ++ desc
    a <- (newListArray (0,length lst)
        $ map (fromIntegral. fromEnum) lst:: IO B)
    unsafeWrite a (length lst) 0
    pr a
    make' n 0
    where
        make' :: Int -> Int -> IO ()
        make' !n !i = do
            let line = (unsafePerformIO $
                        newArray (0,60) 0 :: B)
            if n > 0
                then do
                    !c <- f
                    unsafeWrite line i c
                    if i+1 >= 60
                        then do
                            pr line
                            make' (n-1) 0
                        else
                            make' (n-1) (i+1)
                else do
                    unsafeWrite line i 0
                    l <- len line
                    if l /= 0
                        then pr line
                        else return ()

pr :: B -> IO ()
pr line = withStorableArray line (\ptr -> puts ptr)
len :: B -> IO CInt
len line  = withStorableArray line (\ptr -> strlen ptr)

repeat :: A -> Int -> IO Word8
repeat xs !n = do
        let v = unsafePerformIO $ newIORef 0
        !i <- readIORef v
        if i+1 >= n
            then writeIORef v 0
            else writeIORef v (i+1)
        return $ xs `unsafeAt` i

random :: C -> IO Word8
random ab = do
  seed <- readIORef last
  let !(W newseed val) = genRandom seed ab
  writeIORef last newseed
  return val
 where
  last = unsafePerformIO $ newIORef 42

data W = W {-# UNPACK #-} !Int {-# UNPACK #-} !Word8

genRandom :: Int -> C -> W
genRandom seed (!a,!b) = find 0
  where find i
            | b `unsafeAt` i >= rnd = W newseed (a `unsafeAt` i)
            | otherwise = find (i+1)
        D newseed rnd = genRand seed

data D = D {-# UNPACK #-} !Int {-# UNPACK #-} !Double

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

alu    :: [Char]
alu =
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
    \GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
    \CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
    \ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
    \GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
    \AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
    \AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

mkCum :: [(Char,Double)] -> [(Word8,Double)]
mkCum lst = map (\(c,p) -> ((fromIntegral.fromEnum) c,p)) $
              scanl1 (\(_,p) (c',p') -> (c', p+p')) lst

homosapiens, iub :: C

iub' = mkCum [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homosapiens' = mkCum [('a',0.3029549426680),('c',0.1979883004921)
                ,('g',0.1975473066391),('t',0.3015094502008)]

iub = (listArray (0, (length iub')-1) $ map fst iub',
        listArray (0, (length iub')-1) $ map snd iub')

homosapiens = (listArray (0, (length homosapiens')-1) $ map fst homosapiens',
                listArray (0, (length homosapiens')-1) $ map snd homosapiens')
