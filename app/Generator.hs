module Generator where

import ArgParser
import Data.List (intercalate)
import Alphabet
import Repeater

bip39_min_bytes :: Int
bip39_min_bytes = 32        -- 256 bits of hash, additional 8 bit as a checksum, 33 bytes total

joinStrings :: [String] -> String
joinStrings = intercalate ","

generatorBip39 :: [AlphabetType] -> Maybe Int -> String -> String
generatorBip39 _ _ password = do
    let repeated = getRepeated password bip39_min_bytes
        hashed = getHashed repeated bip39_min_bytes
        checksumAppended = getChecksumAndAppend hashed
        base2048 = convertLargeByteStringToBase 2048 checksumAppended
    joinStrings $ map (bip39List !!) base2048
    

generatorNormal :: [AlphabetType] -> Maybe Int -> String -> String
generatorNormal alphabetTypes (Just length) password = do
    let repeated = getRepeated password length
        hashed = getHashed repeated length
        base = convertLargeByteStringToBase length hashed
        allChars = alphabet alphabetTypes
        chars = map (allChars !!) base
    foldl (++) "" chars


generator :: Args -> String
generator (Args alphabetTypes length password) = do
    case alphabetTypes of
        [Bip39] -> generatorBip39 [Bip39] length password
        _       -> generatorNormal alphabetTypes length password