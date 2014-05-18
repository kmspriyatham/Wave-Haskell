module WAV (
        Wave,
        readWave,
        writeWave
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as DL (intercalate)
import Data.Word

data Wave = Wave {
    numberOfChannels :: Int,
    sampleRate :: Int,
    byteRate :: Int,
    blockAlign :: Int,
    bitsPerSample :: Int,
    waveData :: L.ByteString
} deriving (Eq)

instance Show Wave where
    show (Wave nc sr br ba bps _) = "Wave " ++ DL.intercalate " " (map show [nc, sr, br, ba, bps])

readWave :: FilePath -> IO Wave

readWave filepath = do
    content <- L.readFile filepath
    let w = parseWave content
    case w of
        Just wave -> return wave
        Nothing -> error (filepath ++ ": Not a WAVE file")

writeWave :: FilePath -> Wave -> IO()

writeWave filepath wave = do
    let s = unParseWave wave
    L.writeFile filepath s

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
    matchMagic "RIFF" s                     >>=
    extractDrop 4                           >>=
    \(chunkSize, x) -> matchMagic "WAVE" x  >>=
    matchMagic "fmt "                       >>=
    extractDrop 4                           >>=
    \(subChunk1Size, x) -> extractDrop 2 x  >>=
    \(audioFormat, x) -> extractDrop 2 x    >>=
    \(nc, x) -> extractDrop 4 x             >>=
    \(sr, x) -> extractDrop 4 x             >>=
    \(br, x) -> extractDrop 2 x             >>=
    \(ba, x) -> extractDrop 2 x             >>=
    \(bps, x) -> matchMagic "data" x        >>=
    extractDrop 4                           >>=
    \(subChunk2Size, wd) -> Just (Wave nc sr br ba bps wd)

unParseWave :: Wave -> L.ByteString

unParseWave (Wave nc sr br ba bps wd) =
    L.concat ([L8.pack "RIFF", intToBS (36 + lwd, 4), L8.pack "WAVE", L8.pack "fmt "] ++
              map intToBS [(16, 4), (1, 2), (nc, 2), (sr, 4), (br, 4), (ba, 2), (bps, 2)] ++
              [L8.pack "data", intToBS (lwd, 4), wd])
    where lwd = fromIntegral (L.length wd)

intToBS :: (Int, Int) -> L.ByteString

intToBS (n, count) = L.pack (a ++ (take (count - (length a))(repeat 0x0)))
                     where a = map fromIntegral ((\(q, r) -> [r, q]) $ quotRem n 256)