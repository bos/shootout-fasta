{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Word (Word8, Word64)
import qualified Data.Vector.Mutable as MV
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
    , _entCount :: {-# UNPACK #-} !(ForeignPtr Int)
    , _entNext :: Entry
    } | End
           deriving (Eq, Show)

type Table = IORef MTable

data MTable = MTable {
      mtSize :: {-# UNPACK #-} !(ForeignPtr Int)
    , mtEntries :: MV.IOVector Entry
    }

newTable :: IO Table
newTable = do
  ents <- MV.new 1024
  MV.set ents End
  size <- mallocForeignPtr
  withForeignPtr size (`poke` 0)
  newIORef (MTable size ents)

update :: Table -> Word64 -> IO ()
update mt k = do
  MTable{..} <- readIORef mt
  let len = MV.length mtEntries
      bucket = fromIntegral k `mod` len
  v <- MV.unsafeRead mtEntries bucket
  let go End = do nc <- mallocForeignPtr
                  withForeignPtr nc (`poke` 1)
                  MV.unsafeWrite mtEntries bucket (Entry k nc v)
                  withForeignPtr mtSize $ \ptr -> do
                    newSize <- (+1) `fmap` peek ptr
                    poke ptr newSize
                    when (newSize > (len `div` 4) * 3) $
                      resize mt mtSize =<< V.unsafeFreeze mtEntries
      go (Entry ek ec en) | ek /= k = go en
                          | otherwise = withForeignPtr ec $ \p ->
                                          poke p . (+1) =<< peek p
  go v

resize :: Table -> ForeignPtr Int -> V.Vector Entry -> IO ()
resize mt fp oldEnts = do
  let newLen = V.length oldEnts * 2
  ents <- MV.new newLen
  MV.set ents End
  V.forM_ oldEnts $ \es ->
    let go End              = return ()
        go (Entry ek ec en) = do
            let b = fromIntegral ek `mod` newLen
            MV.unsafeWrite ents b . Entry ek ec =<< MV.unsafeRead ents b
            go en
    in go es
  writeIORef mt (MTable fp ents)

toList :: Table -> IO [(Word64, Int)]
toList mt = do
  MTable{..} <- readIORef mt
  let go End = return []
      go (Entry ek ec en) = do
              c <- withForeignPtr ec peek
              ((ek, c) :) `fmap` go en
  (fmap concat . mapM go . V.toList) =<< V.freeze mtEntries
