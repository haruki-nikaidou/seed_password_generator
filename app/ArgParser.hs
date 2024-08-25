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


alphabetParser :: Parser ([AlphabetType], Maybe Int)
alphabetParser = option (eitherReader parseAlphabetAndLength)
  (  long "type"
  <> short 't'
  <> metavar "TYPE[LENGTH]"
  <> help "Alphabet type (l:lowercase, u:uppercase, n:numbers, s:symbols, bip39) optionally followed by length"
  )


parseAlphabetAndLength :: String -> Either String ([AlphabetType], Maybe Int)
parseAlphabetAndLength s =
  case span (/= 'b') (map toLower s) of
    (_, "bip39") -> Right ([Bip39], Nothing)
    (types, rest) ->
      let validTypes = nub $ map charToAlphabetType types
      in if null validTypes
         then Left "Invalid alphabet type"
         else case reads rest of
           [(n, "")] -> Right (validTypes, Just n)
           _         -> Right (validTypes, Nothing)

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


argsParser :: Parser Args
argsParser = (Args . fst <$> alphabetParser)
  <*> (snd <$> alphabetParser)
  <*> passwordParser