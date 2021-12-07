module Main where

import B hiding (Parser)
import qualified Data.Text.IO as T
import Text.Pretty.Simple
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    run $ head args


run :: String -> IO ()
run infile = do
  program <- T.readFile infile
  case (runParser programP infile program) of
    Left  err -> putStrLn $ errorBundlePretty err
    Right ast -> do
        putDoc $ pretty ast
        putStrLn "^^^ Parser: OK ^^^"
        case (checkProgram ast) of
            Left err -> putDoc $ pretty err <> "\n"
            Right sast -> pPrint sast
