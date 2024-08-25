
module Repeater where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)
import Data.ByteString.Builder (word64BE, toLazyByteString)
import Data.ByteString.Lazy (toStrict)

getRepeated :: String -> Int -> BS.ByteString
getRepeated s minBytes = 
    let bytes = BSC.pack s
        times = minBytes `div` BS.length bytes
        remainder = minBytes `mod` BS.length bytes
    in BS.concat $ replicate times bytes ++ [BS.take remainder bytes]

getHashed :: BS.ByteString -> Int -> BS.ByteString
getHashed s minBytes =
    let minHashTimes = minBytes `div` 32
        mustEndWith = toStrict $ toLazyByteString $ word64BE $ fromIntegral minBytes
        initialHash = SHA256.hash s
        firstSerialHashes = BS.concat $ replicate minHashTimes initialHash
        
        hashUntilMatch accHashes =
            let newHash = SHA256.hash accHashes
            in if BS.isSuffixOf mustEndWith newHash
               then BS.take minBytes $ BS.append accHashes newHash
               else hashUntilMatch $ BS.append accHashes newHash
    in hashUntilMatch firstSerialHashes


word8ToInt :: Word8 -> Int
word8ToInt = fromInteger . toInteger

chunkToInt :: BS.ByteString -> Int
chunkToInt = BS.foldl' (\acc byte -> (acc `shiftL` 8) .|. word8ToInt byte) 0

intoChunks :: BS.ByteString -> [Int]
intoChunks bs
    | BS.null bs = []
    | otherwise  = chunkToInt chunk : intoChunks rest
  where
    (chunk, rest) = BS.splitAt 1 bs


toBaseX :: [Int] -> Int -> [Int]
toBaseX chunks base = reverse $ go chunks 0 []
  where
    go [] carry acc
        | carry == 0 = acc
        | otherwise  = carry : acc
    go (x:xs) carry acc =
        let total = x + carry * (256) 
            (newCarry, remainder) = divMod total base
        in go xs newCarry (remainder : acc)


convertLargeByteStringToBase :: Int -> BS.ByteString -> [Int]
convertLargeByteStringToBase base bs
    | base < 2 || base > 16384 = error "Base must be between 2 and 16384"
    | otherwise = toBaseX (intoChunks bs) base


bip39Checksum :: BS.ByteString -> BS.ByteString     -- get 8 bits of checksum
bip39Checksum bs = 
    let hash = SHA256.hash bs
    in BS.take 1 hash

getChecksumAndAppend :: BS.ByteString -> BS.ByteString
getChecksumAndAppend bs =
    let checksum = bip39Checksum bs
    in BS.append checksum bs