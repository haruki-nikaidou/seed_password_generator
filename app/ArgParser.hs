module ArgParser where

import Options.Applicative
import Alphabet
import Data.Char (toLower)
import Data.List (nub)

data Args = Args
  { alphabetTypes :: [AlphabetType],
    length :: Maybe Int,
    userPassword :: String
  }
  deriving (Show)


argsParser :: Parser Args
argsParser = Args
  <$> alphabetTypeParser
  <*> optional lengthParser
  <*> passwordParser

alphabetTypeParser :: Parser [AlphabetType]
alphabetTypeParser = option (eitherReader parseAlphabetType)
  (  long "type"
  <> short 't'
  <> metavar "TYPE"
  <> help "Alphabet type (l:lowercase, u:uppercase, n:numbers, s:symbols, bip39)")

parseAlphabetType :: String -> Either String [AlphabetType]
parseAlphabetType s = 
  case map toLower s of
    "bip39" -> Right [Bip39]
    types   -> Right $ nub $ map charToAlphabetType types


charToAlphabetType :: Char -> AlphabetType
charToAlphabetType 'l' = Lowercase
charToAlphabetType 'u' = Uppercase
charToAlphabetType 'n' = Numbers
charToAlphabetType 's' = Symbols
charToAlphabetType _   = error "Invalid alphabet type"

passwordParser :: Parser String
passwordParser = strArgument
  (  metavar "PASSWORD"
  <> help "User-provided password for generation"
  )


lengthParser :: Parser Int
lengthParser = option auto
  (  long "length"
  <> short 'l'
  <> metavar "LENGTH"
  <> help "Length of the generated password (not applicable for BIP39)")