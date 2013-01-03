{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import Data.Word (Word8, Word64)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Foreign.ForeignPtr
import Foreign.Storable
import Data.IORef
import Control.Monad
import Data.Bits
import qualified Test.QuickCheck as Q
import Data.Char
import Debug.Trace
import Control.Arrow
import Data.Tuple
import Data.List
import Text.Printf

-- [(97,3787075),(99,2474784),(103,2469314),(116,3768827)]
-- [(0,3787075), (2,2474783),   (6,2469313), (4,3768827)]

-- [(0,1147162),(2,750076),(4,1141419),(6,748416),(8,750267),(10,489579),(12,746365),(14,488572),(16,1141599),(18,746318),(20,1136412),(22,744497),(24,748045),(26,488810),(28,744631),(30,487828)]

-- [(29793,1141600),(29795,746318),(29799,744497),(29812,1136412),(24929,1147162),(24931,750078),(24935,748416),(24948,1141419),(25441,750267),(26465,748046),(26467,488809),(25443,489579),(25447,488572),(26471,487829),(25460,746365),(26484,744631)]

main = do
  genome <- readGenome
  bySize 1 genome
  bySize 2 genome
  mapM_ (`specific` genome) ["GGT","GGTA","GGTATT","GGTATTTTAATT","GGTATTTTAATTTATAGT"]


compressPrefix :: Int -> B.ByteString -> Word64
{-# INLINE compressPrefix #-}
compressPrefix k bs = go 0 0
    where go i !p | i' < k    = go i' (compress k bs i p)
                  | otherwise = p
                  where i' = i+1

-- a 0 -- c 2 -- t 4 -- g 6

compress :: Int -> B.ByteString -> Int -> Word64 -> Word64
{-# INLINE compress #-}
compress k genome off prev = ((prev `shiftL` 2) .&. mask) .|.
                             fromIntegral ((genome `B.unsafeIndex` off) .&. 6)
    where mask = (1 `shiftL` ((k `shiftL` 1) + 1)) - 1

uncompress :: Int -> Word64 -> String
uncompress k = reverse . go 0
    where go i w | i == k = []
                 | otherwise = c : go (i+1) (w `shiftR` 2)
                 where c = "A_C_T_G" `B8.index` fromIntegral (w .&. 6)

newtype Genome = Genome { fromGenome :: B.ByteString }

instance Show Genome where
    show = show . fromGenome

smallish :: Q.Gen a -> Q.Gen a
smallish g = Q.sized $ \n -> Q.resize (small n) g
  where small = round . (sqrt :: Double -> Double) . fromIntegral

instance Q.Arbitrary Genome where
    arbitrary = smallish $ do
                  Q.Positive k <- Q.arbitrary
                  (Genome . B8.pack) `fmap` Q.vectorOf k (Q.elements "acgtACGT")

t_compress_uncompress (Genome genome) = do
  let genlen = B.length genome
  k <- Q.choose (1, min genlen 30)
  let seq = B8.map toUpper . B.take k $ genome
      comp = compress k genome (k-1) (compressPrefix k genome)
  return $ B8.pack (uncompress k comp) == seq

compressions :: Int -> B.ByteString -> [Word64]
compressions k genome = go (k-1) $ compressPrefix k genome
    where go i !p | i == B.length genome = []
                  | otherwise            = p' : go (i+1) p'
              where p' = compress k genome i p

sequences :: Int -> B.ByteString -> [B.ByteString]
sequences k = takeWhile ((==k) . B.length) . map (B.take k) . B.tails

t_compress_roundtrip (Genome genome) = do
  let genlen = B.length genome
  k <- Q.choose (1, min genlen 30)
  let comps = compressions k genome
      seqs = map (B8.map toUpper) . sequences k $ genome
  return $ map (B8.pack . uncompress k) comps == seqs

buildTable k genome = do
  ht <- newTable
  let go i !p
         | i < B.length genome = update ht p' >> go (i+1) p'
         | otherwise           = return ht
         where p' = compress k genome i p
  go (k-1) (compressPrefix k genome)

bySize k genome = do
  ht <- buildTable k genome
  xs <- toList ht
  let pct = (100::Float) / (fromIntegral . sum . map snd $ xs)
      wat (a,b) (c,d) = case compare d b of
                          EQ -> compare a c
                          w  -> w
      ys = sortBy wat . map (uncompress k *** ((*pct) . fromIntegral)) $ xs
  forM_ ys $ \(k,f) -> putStrLn $ printf "%s %.03f" k f
  putStr "\n"

specific seq genome = do
  let k = B.length seq
  ht <- buildTable k genome
  n <- htLookup ht (compress k seq (k-1) $ compressPrefix k seq)
  putStrLn $ show n ++ '\t' : B8.unpack seq

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
  let go End = {-# SCC "update/addNew" #-}
               do nc <- mallocForeignPtr
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

htLookup :: Table -> Word64 -> IO Int
htLookup mt k = do
  MTable{..} <- readIORef mt
  let go (Entry ek ec en) | ek /= k   = go en
                          | otherwise = withForeignPtr ec peek
      go End = return 0
  go =<< MV.unsafeRead mtEntries (fromIntegral k `mod` MV.length mtEntries)

htSize :: Table -> IO Int
htSize mt = do
  MTable{..} <- readIORef mt
  withForeignPtr mtSize peek

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
