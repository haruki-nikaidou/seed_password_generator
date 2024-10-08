module Generator where

import ArgParser hiding (passwordLength, alphabetTypes)
import Data.List (intercalate)
import Alphabet
import Repeater
    ( getRepeated,
      getHashed,
      convertLargeByteStringToBase,
      getChecksumAndAppend )


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
generatorNormal alphabetTypes (Just passwordLength) password = do
    let repeated = getRepeated password passwordLength
        hashed = getHashed repeated passwordLength
        allChars :: [String]
        allChars = alphabet alphabetTypes
        base = convertLargeByteStringToBase (length allChars) hashed
        chars = map (allChars !!) base
    concat chars

generatorNormal _ Nothing _ = error "Length must be provided for normal alphabet types"


generator :: Args -> String
generator (Args alphabetTypes passwordLength password) = do
    case alphabetTypes of
        [Bip39] -> generatorBip39 [Bip39] passwordLength password
        _       -> generatorNormal alphabetTypes passwordLength password