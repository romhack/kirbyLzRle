module Main where

import           Control.Monad (replicateM)
import           Data.Binary.Bits.Get (BitGet, runBitGet, getWord8, getWord16be)
import           Data.Binary.Bits.Put (runBitPut, putWord8, putWord16be)
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import           Data.Bits ((.&.), shiftL, (.|.), shiftR)
import qualified Data.ByteString.Lazy as Bs
import           Data.Hashable (hash)
import           Data.Int (Int64)
import           Data.List (elemIndices)
import           Data.List.Split (chunksOf, divvy)
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Word (Word8)
import           System.Environment (getArgs)
import           Text.Printf (printf)

data Entry = Raw [Word8]
           | Rle8 { len :: Int, byte :: Word8 }
           | Rle16 { len :: Int, word :: [Word8] }
           | RleAsc { len :: Int, start :: Word8 }
           | Lz { len :: Int, dist :: Int }
           | LzInv { len :: Int, dist :: Int }
           | LzRev { len :: Int, dist :: Int }
  deriving (Show, Eq)

instance Ord Entry where
  compare x y  -- compare first by gain, if equal then by largest decode size
    | entryGain x == entryGain y = comparing entryDecodeSize x y
    | otherwise = comparing entryGain x y

entryGain :: Entry -> Int
entryGain (Raw raws)
  | null raws = error "trying entryGain of empty raws"
  | length raws == 1 = -1
  | length raws == (extLen + 1) = -1
  | otherwise = 0 --usual adding of raw - zero gain
entryGain (Lz l _) =
  if l > extLen
    then l - 4
    else l - 3
entryGain (LzInv l _) = entryGain (Lz l 0)
entryGain (LzRev l _) = entryGain (Lz l 0)
entryGain (Rle8 l _) =
  if l > extLen
    then l - 3
    else l - 2
entryGain (Rle16 l _) =
  if l > extLen
    then l * 2 - 4
    else l * 2 - 3
entryGain (RleAsc l _) = entryGain (Rle8 l 0)

entryDecodeSize :: Entry -> Int -- to how many bytes this entry decodes in buffer
entryDecodeSize (Raw bs) = length bs
entryDecodeSize (Rle16 l _) = l * 2
entryDecodeSize e = len e

windowSize :: Int
windowSize = 0x10000 -- as distance can be encoded only by 16 bits

maxLen :: Int
maxLen = 0x400 --extended length encoded by 10 bits

extLen :: Int
extLen = 0x20

main :: IO ()
main = getArgs >>= parse
  where
    parse ["-v"] =
      putStrLn "kirbyLzRle v0.1\nMixed LZ-RLE compression tool \
      \for HAL Laboratory games."
    parse ["-d", inFileName, offs, outFileName] =
      decompress inFileName (read offs) outFileName
    parse ["-c", inFileName, outFileName] = compress inFileName outFileName
    parse _ =
      putStrLn
        "Usage:\n\
      \  kirbyLzRle -d <inFile> <offset> <outFile>  Decompress block from given ROM file.\n\
      \  kirbyLzRle -c <inFile> <outFile> Compress given plain block.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."

decompress :: String -> Int64 -> String -> IO ()
decompress inFileName offs outFileName = do
  input <- Bs.readFile inFileName
  let binaryTail = Bs.drop offs input
      entriesResult = G.runGetOrFail (runBitGet deserialize) binaryTail
  case entriesResult of
    Left (_, _, errStr) -> error $ "Entries parsing error: " ++ errStr
    Right (_, size, entries) -> do
      Bs.writeFile outFileName $ Bs.pack (decode entries)
      putStrLn $ printf "Compressed block size was 0x%X" size

deserialize :: BitGet [Entry] --read input bytes into entries records
deserialize = do
  initialCode <- getWord8 3
  if initialCode == 7
    then do
      --extended code found
      code <- getWord8 3
      if code == 7
        then return [] --0xFF end of stream
        else do
          l <- getWord16be 10  --extended length
          e <- getEntryData code (fromIntegral l + 1) --zero based length
          rest <- deserialize
          return $ e : rest
    else do
      --normal length entry
      l <- getWord8 5
      e <- getEntryData initialCode (fromIntegral l + 1)
      rest <- deserialize
      return $ e : rest

getEntryData :: Word8 -> Int -> BitGet Entry --construct entry reading it's data
getEntryData code l =
  case code of
    0 -> Raw <$> replicateM l (getWord8 8)
    1 -> Rle8 l <$> getWord8 8
    2 -> Rle16 l <$> replicateM 2 (getWord8 8)
    3 -> RleAsc l <$> getWord8 8
    4 -> Lz l <$> (fromIntegral <$> getWord16be 16)
    5 -> LzInv l <$> (fromIntegral <$> getWord16be 16)
    6 -> LzRev l <$> (fromIntegral <$> getWord16be 16)
    _ -> error "Unknown code in getEntryData"

decode :: [Entry] -> [Word8] --decode entries list into plain block bytes
decode = foldl go []
  where
    go buffer e = buffer ++ decodeEntry e
      where
        decodeEntry (Raw bs) = bs
        decodeEntry (Rle8 l b) = replicate l b
        decodeEntry (Rle16 l ws) = concat (replicate l ws)
        decodeEntry (RleAsc l s) = take l [s ..]
        decodeEntry (Lz l d) = take l (cycle' (drop d buffer)) --infinite cycle list
        decodeEntry (LzInv l d) =
          map invertBitsOrder $ take l (cycle' (drop d buffer))
        decodeEntry (LzRev l d) = take l revBufferTail
          where
            revBufferTail = drop (length buffer - d - 1) $ reverse buffer
        --If length>distance, we will read data, already decoded in this reference
        cycle' xs =
          if null xs
            then xs
            else cycle xs --cycle, that works with empty lists

compress :: String -> String -> IO () --compress filename
compress inputName outName = do
  inputBs <- Bs.readFile inputName
  let input = Bs.unpack inputBs
      encodedBlock = (serialize . encode) input
  Bs.writeFile outName encodedBlock

encode :: [Word8] -> [Entry] -- encode plain block bytes into compression entries
encode haystack' = getEntries 0 [] --start from start of haystack and empty rawBuffer
  where
    haystack = take windowSize haystack' --distance can be encoded by 16 bits
    haystackHashes = map hash $ divvy 3 1 haystack --split by chunks of 3 bytes and hash
    haystackInvertedBits = map invertBitsOrder haystack
    haystackHashesInv = map hash $ divvy 3 1 haystackInvertedBits
    haystackReversed = reverse haystack
    haystackHashesRev = map hash $ divvy 3 1 haystackReversed
    getEntries :: Int -> [Word8] -> [Entry] --process whole haystack for entries
    getEntries pos raws
      | pos >= length haystack = [Raw raws]
      | entryGain curPosEntry > entryGain currentRawEntry &&
        entryGain curPosEntry >=
        entryGain currentRawEntry + entryGain skipPosEntry =
          Raw raws :
          curPosEntry : --emit accumulated raws, current entry and nullify rawbuffer
          getEntries (pos + entryDecodeSize curPosEntry) []
      | otherwise =
          if length currentRaws <= maxLen -- no options better, than raw
            then getEntries (pos + 1) currentRaws --dump current raw to output
            else currentRawEntry : getEntries (pos + 1) [] --exceeded max len, break raws chunk
      where
        curPosEntry = fromMaybe currentRawEntry $ getEntry pos
        skipPosEntry = fromMaybe skipRawEntry $ getEntry (pos + 1)
        skipRawEntry = Raw (currentRaws ++ [haystack !! (pos + 1)])
        currentRaws = raws ++ [haystack !! pos]
        currentRawEntry = Raw currentRaws
    getEntry :: Int -> Maybe Entry --get best entry at current position
    getEntry pos =
      if null candidates
        then Nothing
        else Just $ maximum candidates
      where
        candidates =
          concat [findLzInv, findLzRev, findLz, findRle16, findRleAsc, findRle8]
        needle = take maxLen $ drop pos haystack
        currentRaw = head needle
        needleHash = hash $ take 3 needle
        findLz :: [Entry] --Nothing, if not found.
        findLz = map getLzPair indices
          where
            getLzPair distance = Lz prefixLength distance
              where
                -- get longest common substring in two lists compare with full 'haystack' here for length outbound
                -- of buffer
                prefixLength =
                  length $
                    takeWhile (== True) $
                      zipWith (==) needle (drop distance haystack)
            indices = elemIndices needleHash (take pos haystackHashes)
        --indexes of probable matches. Search only through unpacked buffer hashes
        findLzInv :: [Entry] --Nothing, if not found.
        findLzInv = map getLzPair indices
          where
            getLzPair distance = LzInv prefixLength distance
              where
                -- get longest common substring in two lists compare with full 'haystack' here for length outbound
                -- of buffer
                prefixLength =
                  length $
                    takeWhile (== True) $
                      zipWith (==) needle (drop distance haystackInvertedBits)
            indices = elemIndices needleHash (take pos haystackHashesInv)
        --indexes of probable matches. Search only through unpacked buffer hashes
        findLzRev :: [Entry] --Nothing, if not found.
        findLzRev = map getLzPair indices
          where
            getLzPair distance = LzRev prefixLength (pos - distance - 1)
              where
                prefixLength =
                  length $
                    takeWhile (== True) $
                      zipWith (==) needle (drop (positionRev + distance) haystackReversed)
            indices =
              elemIndices needleHash (drop positionRev haystackHashesRev)
            --indexes of probable matches. Search only through unpacked buffer hashes
            positionRev = length haystackReversed - pos --we start at reversed here
        findRle8 :: [Entry] --Nothing, if not found.
        findRle8 =
          if length equals < 2
            then [] --0 gain at 2byte rle
            else [Rle8 (length equals) currentRaw]
          where
            equals = takeWhile (== currentRaw) needle
        findRle16 :: [Entry]
        findRle16 =
          if length equals < 2
            then [] --0 gain at 2 times repeat rle
            else [Rle16 (length equals) currentWord]
          where
            needle' = chunksOf 2 $ take (maxLen * 2) $ drop pos haystack --two bytes chunks
            currentWord = head needle'
            equals = takeWhile (== currentWord) needle'
        findRleAsc :: [Entry]
        findRleAsc =
          if length ascendings < 2
            then [] --0 gain at 2byte rle
            else [RleAsc (length ascendings) currentRaw]
          where
            ascendings = takeAsc needle
            takeAsc [] = [] --take ascending (+1) part of the list
            takeAsc [x] = [x]
            takeAsc (x:y:xs) =
              if y == x + 1
                then x : takeAsc (y : xs)
                else [x]

invertBitsOrder :: Word8 -> Word8 -- used in LzRev compression command
invertBitsOrder x =
  ((tbl !! fromIntegral (x .&. 0x0F)) `shiftL` 4) .|.
  tbl !! fromIntegral (x `shiftR` 4)
  where
    tbl =
      [0x0, 0x8, 0x4, 0xc, 0x2, 0xa, 0x6, 0xe, 0x1, 0x9, 0x5, 0xd, 0x3, 0xb, 0x7, 0xf]

serialize :: [Entry] -> Bs.ByteString -- serialize list of entries to compressed bytes
serialize es = block `Bs.snoc` 0xFF --ff terminated block
  where
    block = P.runPut . runBitPut $ mapM_ putEntry (filter (/= Raw []) es)
    putEntry e =
      case e of
        Raw bs        -> putLenChunk 0 >> mapM_ (putWord8 8) bs
        Rle8 _ b      -> putLenChunk 1 >> putWord8 8 b
        Rle16 _ wList -> putLenChunk 2 >> mapM_ (putWord8 8) wList
        RleAsc _ s    -> putLenChunk 3 >> putWord8 8 s
        Lz _ d        -> putLenChunk 4 >> putWord16be 16 (fromIntegral d)
        LzInv _ d     -> putLenChunk 5 >> putWord16be 16 (fromIntegral d)
        LzRev _ d     -> putLenChunk 6 >> putWord16be 16 (fromIntegral d)
      where
        putLenChunk entryCode =
          if serialLen > extLen
            then do
              --extended case
              putWord8 3 7 --extension code b111
              putWord8 3 entryCode
              putWord16be 10 $ fromIntegral serialLen
            else do
              -- normal length case
              putWord8 3 entryCode
              putWord8 5 $ fromIntegral serialLen
        serialLen =
          case e of
            (Raw bs) -> length bs - 1
            _        -> len e - 1
