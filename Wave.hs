module Wave (
        Wave(..),
        readWave,
        writeWave
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.List as DL (intercalate)
import ByteParse

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
    let w = runParse waveParser (ParseState content 0)
    case w of
        Right (w, l) -> return w
        Left err -> error err

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

unParseWave :: Wave -> L.ByteString
unParseWave (Wave c1s af nc sr br ba bps c2s wd) =
    L.concat ([L8.pack "RIFF", intToBS (20 + c1s + c2s) 4, L8.pack "WAVE", L8.pack "fmt "] ++
              map (uncurry intToBS) [(c1s, 4), (compressionToFormat af, 2), (nc, 2), (sr, 4), (br, 4), (ba, 2), (bps, 2)] ++
              [L8.pack "data", intToBS c2s 4, wd])

waveParser :: Parse Wave
waveParser = do
    getMagic "RIFF" 
    cs  <- getNumber 4     
    getMagic "WAVE" 
    getMagic "fmt " 
    c1s <- getNumber 4     
    af  <- getNumber 2     
    nc  <- getNumber 2     
    sr  <- getNumber 4     
    br  <- getNumber 4     
    ba  <- getNumber 2     
    bps <- getNumber 2     
    getMagic "data" 
    c2s <- getNumber 4     
    wd  <- getBytes c2s 
    return (Wave c1s (formatToCompression af) nc sr br ba bps c2s wd)