module ByteParse (
  ParseState(..),
  Parse(..),
  getNumber,
  getMagic,
  getBytes,
  setMagic,
  setNumber,
  setBytes
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

hexToDec :: [Int] -> Int
hexToDec xs = fromIntegral $ sum $ zipWith (*) (map (256^) [0..]) xs

decToHex :: Int -> Int -> Maybe L.ByteString
decToHex n count = if count >= length a
                      then Just (L.pack (a ++ (take (count - (length a))(repeat 0))))
                      else Nothing
                  where a = map fromIntegral (to256 n)
                        to256 n = if n < 256
                                    then [n]
                                    else (rem n 256):to256 (div n 256)

data ParseState = ParseState {
    byteString :: L.ByteString
}

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

instance Monad Parse where
    firstParse >>= secondParse = Parse compositeParse
         where compositeParse initState =
                case runParse firstParse initState of
                    Left errorMessage -> Left errorMessage
                    Right (firstResult, nextState) -> runParse (secondParse firstResult) nextState
    return a = Parse (\ s -> Right (a, s))

getNumber :: Int -> Parse Int
getNumber count = Parse f
    where n = fromIntegral count
          f ps
            | L.length t == n = Right (hexToDec (map fromIntegral $ L.unpack t), ParseState l)
            | otherwise = Left "Header doesn't contain sufficient number of bytes"
              where (t, l) = L.splitAt n $ byteString ps

getMagic :: String -> Parse ()
getMagic s = Parse f
    where f ps
            | L8.isPrefixOf pm bs = Right ((), ParseState (L.drop lpm bs))
            | otherwise = Left ("Header doesn't contain the magic word: " ++ s)
              where bs = byteString ps
                    pm = L8.pack s
                    lpm = L.length pm

getBytes :: Int -> Parse L.ByteString
getBytes count = Parse f
    where n = fromIntegral count
          f ps
              | L.length bs < n = Left "Data chunk contains less bytes than expected"
              | otherwise = Right (t, ParseState l)
                where bs = byteString ps
                      (t, l) = L.splitAt n bs

(+++) :: L.ByteString -> L.ByteString -> L.ByteString
s1 +++ s2 = L.concat [s1, s2]

setMagic :: String -> Parse ()
setMagic s = Parse f
    where f ps = Right ((), ParseState (byteString ps +++ l8ps))
          l8ps = L8.pack s

setNumber :: Int -> Int -> Parse ()
setNumber n count = Parse f
    where f ps = case decToHex n count of
                    Just bsn -> Right ((), ParseState (byteString ps +++ bsn))
                    Nothing -> Left "Number too large to represent in given number of bytes"

setBytes :: L.ByteString -> Parse ()
setBytes bs = Parse f
    where f ps = Right ((), ParseState (byteString ps +++ bs))