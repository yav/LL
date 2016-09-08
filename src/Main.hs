module Main(main) where

import System.Environment
import System.IO
import qualified Data.Text.IO as Text

import Lexer
import Parser


main :: IO ()
main = mapM_ checkFile =<< getArgs


checkFile :: FilePath -> IO ()
checkFile file =
  do txt <- Text.readFile file
     let tokens = lexer txt
     case parseDecls tokens of
       Left err -> hPutStrLn stderr err
       Right a  -> print a

