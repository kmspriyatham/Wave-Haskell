module ByteParse (
	ParseState(..),
	Parse(..),
	getNumber,
	getMagic,
	getBytes,
	intToBS
	) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

listOfHexToDec :: [Int] -> Int
listOfHexToDec xs = fromIntegral $ sum $ zipWith (*) (map (256^) [0..]) xs

intToBS :: Int -> Int -> L.ByteString
intToBS n count = L.pack (a ++ (take (count - (length a))(repeat 0)))
                     where a = map fromIntegral ((\ (q, r) -> [r, q]) $ quotRem n 256)

data ParseState = ParseState {
    byteString :: L.ByteString,
    offset :: Int
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
            | L.length t == n = Right (listOfHexToDec (map fromIntegral $ L.unpack t), ParseState l (offset ps + count))
            | otherwise = Left "Header doesn't contain sufficient number of bytes"
              where (t, l) = L.splitAt n $ byteString ps

getMagic :: String -> Parse ()
getMagic s = Parse f
    where f ps
            | L8.isPrefixOf pm bs = Right ((), ParseState (L.drop lpm bs) (offset ps + fromIntegral lpm))
            | otherwise = Left ("Header doesn't contain the magic word: " ++ s)
              where bs = byteString ps
                    pm = L8.pack s
                    lpm = L.length pm

getBytes :: Int -> Parse L.ByteString
getBytes count = Parse f
    where n = fromIntegral count
          f ps
              | L.length bs < n = Left "Wave data is less than expected"
              | otherwise = Right (t, ParseState l (offset ps + count))
                where bs = byteString ps
                      (t, l) = L.splitAt n bs