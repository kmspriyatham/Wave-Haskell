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
    let wave = runParse waveParser (ParseState content)
    case wave of
        Right (w, _) -> return w
        Left err -> error err

writeWave :: FilePath -> Wave -> IO()
writeWave filepath wave = do
    let content = unParseWave wave
    case content of
        Right (_, c) -> L.writeFile filepath (byteString c)
        Left err -> error err

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

unParseWave :: Wave -> Either String ((), ParseState)
unParseWave wave = runParse unParse' (ParseState L.empty)
    where unParse' = do
            setMagic "RIFF"
            setNumber (20 + chunk1Size wave + chunk2Size wave) 4
            setMagic "WAVE"
            setMagic "fmt "
            setNumber (chunk1Size wave) 4
            setNumber (compressionToFormat $ compression wave) 2
            setNumber (numberOfChannels wave) 2
            setNumber (sampleRate wave) 4
            setNumber (byteRate wave) 4
            setNumber (blockAlign wave) 2
            setNumber (bitsPerSample wave) 2
            setMagic "data"
            setNumber (chunk2Size wave) 4
            setBytes (waveData wave)

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