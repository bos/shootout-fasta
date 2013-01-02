{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Word (Word8, Word64)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector as V
import Foreign.ForeignPtr
import Foreign.Storable
import Data.IORef
import Control.Monad
import Data.Bits

main = do
  genome <- readGenome
  length1 genome
  length2 genome

length1 genome = do
  ht <- newTable
  let len = B.length genome
      go n | n == len = return ()
           | otherwise = do
          let k = genome `B.unsafeIndex` n
          update ht (fromIntegral k)
          go (n+1)
  go 0
  print =<< toList ht

length2 genome = do
  ht <- newTable
  let len = B.length genome
      go n !k | n == len = return ()
              | otherwise = do
          let k1 = ((k `shiftL` 8) .|. fromIntegral (genome `B.unsafeIndex` n)) .&. 0xffff
          update ht k1
          go (n+1) k1
  go 0 (fromIntegral (B.unsafeHead genome))
  print =<< toList ht

readGenome =
  (B.filter (/=10) . B.dropWhile (/=10) . snd . B.breakSubstring ">TH") `fmap` B.getContents

data Entry = Entry {
      _entKey :: {-# UNPACK #-} !Word64
    , _entCount :: {-# UNPACK #-} !Int
    , _entNext :: Entry
    } | End
           deriving (Eq, Show)

type Table = IORef MTable

data MTable = MTable {
      mtEntries :: MV.IOVector Entry
    , mtCounts :: MU.IOVector Int
    }

newTable :: IO Table
newTable = do
  let size = 1024
  ents <- MV.new size
  MV.set ents End
  cnts <- MU.new (size + 1)
  MU.set cnts 0
  MU.unsafeWrite cnts 0 1
  newIORef (MTable ents cnts)

update :: Table -> Word64 -> IO ()
update mt k = do
  MTable{..} <- readIORef mt
  let len = MV.length mtEntries
      bucket = fromIntegral k `mod` len
  v <- MV.unsafeRead mtEntries bucket
  let go End = do size <- MU.unsafeRead mtCounts 0
                  MU.unsafeWrite mtCounts size 1
                  let newSize = size + 1
                  MU.unsafeWrite mtCounts 0 newSize
                  MV.unsafeWrite mtEntries bucket (Entry k size v)
                  when (newSize > (len `div` 4) * 3) $ do
                    ents <- V.unsafeFreeze mtEntries
                    resize mt ents mtCounts
      go (Entry ek ec en) | ek /= k = go en
                          | otherwise =
                MU.unsafeWrite mtCounts ec . (+1) =<< MU.unsafeRead mtCounts ec
  go v

resize :: Table -> V.Vector Entry -> MU.IOVector Int -> IO ()
resize mt oldEnts oldCnts = do
  let oldLen = V.length oldEnts
      newLen = oldLen * 2
  ents <- MV.new newLen
  MV.set ents End
  cnts <- MU.grow oldCnts oldLen
  V.forM_ oldEnts $ \es ->
    let go End              = return ()
        go (Entry ek ec en) = do
            let b = fromIntegral ek `mod` newLen
            MV.unsafeWrite ents b . Entry ek ec =<< MV.unsafeRead ents b
            go en
    in go es
  writeIORef mt (MTable ents cnts)

toList :: Table -> IO [(Word64, Int)]
toList mt = do
  MTable{..} <- readIORef mt
  let go End = return []
      go (Entry ek ec en) = do
              c <- MU.unsafeRead mtCounts ec
              ((ek, c) :) `fmap` go en
  (fmap concat . mapM go . V.toList) =<< V.freeze mtEntries
