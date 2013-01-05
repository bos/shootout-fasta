{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Unsafe as B
import Data.Word (Word8, Word64, Word32)
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
import qualified Data.Vector.Unboxed.Mutable as MU
import Control.Concurrent
import Data.Function

main = do
  genome <- readGenome
  let specifics = ["GGT","GGTA","GGTATT","GGTATTTTAATT","GGTATTTTAATTTATAGT"]
  xs <- parallel $ [ bySize 1 genome, bySize 2 genome ] ++
                   map (`specific` genome) specifics
  putStr $ concat (map unlines xs)

parallel :: [IO a] -> IO [a]
parallel acts = do
  caps <- getNumCapabilities
  if caps == 1
    then sequence acts
    else do
      mvs <- forM acts $ \act -> do
        var <- newEmptyMVar
        forkIO $ putMVar var =<< act
        return var
      forM mvs takeMVar

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
         | i < B.length genome = update ht p' (+1) >> go (i+1) p'
         | otherwise           = return ht
         where p' = compress k genome i p
  go (k-1) (compressPrefix k genome)

overlappingChunks :: Int -> Int -> B.ByteString -> [B.ByteString]
overlappingChunks size overlap
    | size <= overlap = error "overlappingChunks: size <= overlap"
    | otherwise       = go
  where go bs
          | B.length bs <= size = [bs]
          | otherwise = B.take size bs : go (B.drop (size - overlap) bs)

buildTablePar :: Int -> B.ByteString -> IO Table
buildTablePar k genome = do
  caps <- getNumCapabilities
  let len = B.length genome
      chunkSize
          | len `mod` caps > 0 = x + 1
          | otherwise          = x
          where x = len `div` caps
      chunks = overlappingChunks chunkSize (k-1) genome
  ht <- buildTable k (head chunks)
  ch <- newChan
  putStrLn $ "sizes: " ++ show (map B.length chunks)
  forM_ (tail chunks) $ \chunk ->
    forkIO $ writeChan ch =<< buildTable k chunk
  forM_ (tail chunks) . const $ extend ht =<< readChan ch
  return ht

bySize :: Int -> B.ByteString -> IO [String]
bySize k genome = do
  ht <- buildTable k genome
  xs <- toList ht
  let pct = (100::Float) / (fromIntegral . sum . map snd $ xs)
      wat (a,b) (c,d) = case compare d b of
                          EQ -> compare a c
                          w  -> w
      ys = sortBy wat . map (uncompress k *** ((*pct) . fromIntegral)) $ xs
  return $ (flip map ys $ \(k,f) -> printf "%s %.03f" k f) ++ [""]

specific seq genome = do
  let k = B.length seq
  ht <- buildTable k genome
  n <- htLookup ht (compress k seq (k-1) $ compressPrefix k seq)
  return [show n ++ '\t' : B8.unpack seq]

readGenome =
  (B.filter (/=10) . B.dropWhile (/=10) . snd . B.breakSubstring ">TH") `fmap` B.getContents

data Entry = Entry {
      _entKey :: {-# UNPACK #-} !Word64
    , _entIndex :: {-# UNPACK #-} !Int
    , _entNext :: Entry
    } | End
           deriving (Eq, Show)

type Table = IORef MTable

data MTable = MTable {
      mtSize :: {-# UNPACK #-} !(ForeignPtr Int)
    , mtEntries :: MV.IOVector Entry
    , mtCounts :: MU.IOVector Word32
    }

newTable :: IO Table
newTable = do
  let size = 1024
  ents <- MV.new size
  MV.set ents End
  cnts <- MU.new size
  size <- mallocForeignPtr
  withForeignPtr size (`poke` 0)
  newIORef (MTable size ents cnts)

update :: Table -> Word64 -> (Word32 -> Word32) -> IO ()
{-# INLINE update #-}
update mt k f = do
  MTable{..} <- readIORef mt
  let len = MV.length mtEntries
      bucket = fromIntegral k `mod` len
  v <- MV.unsafeRead mtEntries bucket
  let go End = {-# SCC "update/addNew" #-}
               do withForeignPtr mtSize $ \ptr -> do
                    size <- peek ptr
                    MV.unsafeWrite mtEntries bucket (Entry k size v)
                    MU.unsafeWrite mtCounts size 1
                    let newSize = size + 1
                    poke ptr newSize
                    when (newSize > (len `div` 4) * 3) $ do
                      ents <- V.unsafeFreeze mtEntries
                      resize mt mtSize ents mtCounts
      go (Entry ek ec en) | ek /= k = go en
                          | otherwise = MU.unsafeRead mtCounts ec >>=
                                        MU.unsafeWrite mtCounts ec . f
  go v

htLookup :: Table -> Word64 -> IO Word32
htLookup mt k = do
  MTable{..} <- readIORef mt
  let go (Entry ek ec en) | ek /= k   = go en
                          | otherwise = MU.unsafeRead mtCounts ec
      go End = return 0
  go =<< MV.unsafeRead mtEntries (fromIntegral k `mod` MV.length mtEntries)

htSize :: Table -> IO Int
htSize mt = do
  MTable{..} <- readIORef mt
  withForeignPtr mtSize peek

extend :: Table -> Table -> IO ()
extend ht oht = mapM_ (\(k,v) -> update ht k (+v)) =<< toList oht

resize :: Table -> ForeignPtr Int -> V.Vector Entry -> MU.IOVector Word32 -> IO ()
resize mt fp oldEnts oldCnts = do
  let len = V.length oldEnts
      newLen = len * 2
  ents <- MV.new newLen
  cnts <- MU.grow oldCnts len
  MV.set ents End
  V.forM_ oldEnts $ \es ->
    let go End              = return ()
        go (Entry ek ec en) = do
            let b = fromIntegral ek `mod` newLen
            MV.unsafeWrite ents b . Entry ek ec =<< MV.unsafeRead ents b
            go en
    in go es
  writeIORef mt (MTable fp ents cnts)

toList :: Table -> IO [(Word64, Word32)]
toList mt = do
  MTable{..} <- readIORef mt
  let go End = return []
      go (Entry ek ec en) = do
              c <- MU.unsafeRead mtCounts ec
              ((ek, c) :) `fmap` go en
  (fmap concat . mapM go . V.toList) =<< V.freeze mtEntries
