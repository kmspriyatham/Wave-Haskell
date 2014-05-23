module WAV (
        Wave(..),
        readWave,
        writeWave
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as DL (intercalate)

data Wave = Wave {
    chunk1Size :: Int,
    compression :: Compression,
    numberOfChannels :: Int,
    sampleRate :: Int,
    byteRate :: Int,
    blockAlign :: Int,
    bitsPerSample :: Int,
    chunk2Size :: Int,
    waveData :: L.ByteString
} deriving (Eq)

data Compression = PCM
                   deriving (Eq, Show)

instance Show Wave where
    show (Wave _ af nc sr br ba bps c2s _) = "Wave " ++ show af ++ " " ++ DL.intercalate " " (map show [nc, sr, br, ba, bps, c2s])

readWave :: FilePath -> IO Wave

readWave filepath = do
    content <- L.readFile filepath
    let w = parseWave content
    case w of
        Just wave -> return wave
        Nothing -> error (filepath ++ ": Not a WAVE file")

writeWave :: FilePath -> Wave -> IO()

writeWave filepath wave = do
    L.writeFile filepath (unParseWave wave)

checkWave :: Wave -> Bool

checkWave (Wave c1s af nc sr br ba bps c2s wd) =
    (c2s == fromIntegral (L.length wd) && (br == div (sr*nc*bps) 8) && (ba == div (nc*bps) 8))

formatToCompression :: Int -> Compression

formatToCompression n =
    case n of
        1 -> PCM

compressionToFormat :: Compression -> Int

compressionToFormat c =
    case c of
        PCM -> 1

matchMagic :: String -> L.ByteString -> Maybe L.ByteString

matchMagic m bs
    | L8.isPrefixOf pm bs = Just (L.drop (L.length pm) bs)
    | otherwise = Nothing
    where pm = L8.pack m

extractDrop :: Int -> L.ByteString -> Maybe (Int, L.ByteString)

extractDrop count s = let n = fromIntegral count in
    if L.length (L.take n s) == n
        then Just (listOfHexToDec (map fromIntegral (L.unpack $ L.take n s)), L.drop n s)
        else Nothing

listOfHexToDec xs = sum $ zipWith (*) (map (256^) [0..]) xs

parseWave :: L.ByteString -> Maybe Wave

parseWave s =
    matchMagic "RIFF" s                      >>=
    extractDrop 4                            >>=
    \ (chunkSize, x) -> matchMagic "WAVE" x  >>=
    matchMagic "fmt "                        >>=
    extractDrop 4                            >>=
    \ (c1s, x) -> extractDrop 2 x            >>=
    \ (af, x) -> extractDrop 2 x             >>=
    \ (nc, x) -> extractDrop 4 x             >>=
    \ (sr, x) -> extractDrop 4 x             >>=
    \ (br, x) -> extractDrop 2 x             >>=
    \ (ba, x) -> extractDrop 2 x             >>=
    \ (bps, x) -> matchMagic "data" x        >>=
    extractDrop 4                            >>=
    \ (c2s, wd) -> Just (Wave c1s (formatToCompression af) nc sr br ba bps c2s wd)

unParseWave :: Wave -> L.ByteString

unParseWave (Wave c1s af nc sr br ba bps c2s wd) =
    L.concat ([L8.pack "RIFF", intToBS (20 + c1s + c2s) 4, L8.pack "WAVE", L8.pack "fmt "] ++
              map (uncurry intToBS) [(c1s, 4), (compressionToFormat af, 2), (nc, 2), (sr, 4), (br, 4), (ba, 2), (bps, 2)] ++
              [L8.pack "data", intToBS c2s 4, wd])

intToBS :: Int -> Int -> L.ByteString

intToBS n count = L.pack (a ++ (take (count - (length a))(repeat 0)))
                     where a = map fromIntegral ((\ (q, r) -> [r, q]) $ quotRem n 256)