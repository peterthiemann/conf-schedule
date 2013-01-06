module Main where

import ParseConfigUU (pFile)
import ParseUtils (runFile)
import JsonUtils (makeJson)

import System.Environment (getArgs)
import Control.Monad (when)

import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ 
    fail "Expecting at least one filename as argument"
  mapM_ (\fileName -> do putStrLn ("Parsing file " ++ fileName)
                         ev <- runFile pFile fileName
                         writeFile (fileName ++ ".json") (T.unpack $ makeJson ev)
                         putStrLn ("Writing output file " ++ fileName ++ ".json")
        ) args
