module Main where


import ArgParser
import Generator
import Options.Applicative

main :: IO ()
main = do 
  args <- execParser opts
  putStrLn $ generator args
  where
    opts = info (argsParser <**> helper)
        ( fullDesc
         <> progDesc "Generate a password"
         <> header "password-generator - a simple password generator" )
