{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import qualified Data.HashMap.Strict as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.HashTable.IO as HT
import qualified Data.IntMap.Strict as IM
import Data.Word (Word8, Word64)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Foreign.ForeignPtr
import Foreign.Storable
import Data.IORef
import Control.Monad

main0 = do
  genome <- readGenome
  let m1 = H.fromListWith (+) . map (flip (,) (1::Int)) . B.unpack $ genome
  print m1

main1 = do
  genome <- readGenome
  ht <- HT.new :: IO (HT.BasicHashTable Word8 Int)
  let len = B.length genome
      go n | n == len = return ()
           | otherwise = do
          let k = genome `B.unsafeIndex` n
          HT.insert ht k 1
          go (n+1)
  go 0
  print =<< HT.toList ht

main2 = do
  genome <- readGenome
  let m1 = IM.fromListWith (+) . map (\k -> (fromIntegral k, 1)) . B.unpack $ genome
  print m1

main3 = do
  genome <- readGenome
  ht <- newTable
  let len = B.length genome
      go n | n == len = return ()
           | otherwise = do
          let k = genome `B.unsafeIndex` n
          update ht (fromIntegral k)
          go (n+1)
  go 0
  print =<< toList ht

main = main3

readGenome =
  (B.dropWhile (/=10) . snd . B.breakSubstring ">TH") `fmap` B.getContents

data Entry = Entry {
      entKey :: {-# UNPACK #-} !Word64
    , entCount :: {-# UNPACK #-} !(ForeignPtr Int)
    , entNext :: Entry
    } | End
           deriving (Eq, Show)

type Table = IORef MTable

data MTable = MTable {
      mtSize :: {-# UNPACK #-} !(ForeignPtr Int)
    , mtEntries :: MV.IOVector Entry
    }

newTable :: IO Table
newTable = do
  ents <- MV.new 128
  size <- mallocForeignPtr
  withForeignPtr size (`poke` 0)
  MV.set ents End
  newIORef (MTable size ents)

update :: Table -> Word64 -> IO ()
update mt k = do
  MTable{..} <- readIORef mt
  let len = MV.length mtEntries
      bucket = fromIntegral k `mod` len
  v <- MV.read mtEntries bucket
  let go End = do nc <- mallocForeignPtr
                  withForeignPtr nc (`poke` 1)
                  MV.write mtEntries bucket (Entry k nc v)
      go (Entry ek ec en) | ek /= k = go en
                          | otherwise = withForeignPtr ec $ \p -> do
                                          c <- peek p
                                          poke p (c+1)
  go v
  withForeignPtr mtSize $ \ptr -> do
    s <- peek ptr
    let newSize = s + 1
    poke ptr newSize
    when (v == End && newSize > (len `div` 4) * 3) $
      resize mt mtSize =<< V.unsafeFreeze mtEntries

resize :: Table -> ForeignPtr Int -> V.Vector Entry -> IO ()
resize mt fp oldEnts = do
  let newLen = V.length oldEnts * 2
  ents <- MV.new newLen
  MV.set ents End
  V.forM_ oldEnts $ \es ->
    let go End              = return ()
        go (Entry ek ec en) = do
                         let bucket = fromIntegral ek `mod` newLen
                         v <- MV.read ents bucket
                         MV.write ents bucket (Entry ek ec v)
                         go en
    in go es
  writeIORef mt (MTable fp ents)

toList :: Table -> IO [(Word64, Int)]
toList mt = do
  MTable{..} <- readIORef mt
  let go :: Entry -> IO [(Word64, Int)]
      go End = return []
      go (Entry ek ec en) = do
              c <- withForeignPtr ec peek
              xs <- go en
              return $ (ek, c) : xs
  (fmap concat . mapM go . V.toList) =<< V.freeze mtEntries
