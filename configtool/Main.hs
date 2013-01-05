module Main where

import ParseConfigUU (pFile)
import ParseUtils (runFile)
import ConfigJSON (toJSON)

import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  when (length args == 0) $ 
    fail "Expecting at least one filename as argument"
  mapM_ (\fileName -> do putStrLn ("Parsing file " ++ fileName)
                         ev <- runFile pFile fileName
                         writeFile (fileName ++ ".json") (toJSON ev "")
                         putStrLn ("Writing output file " ++ fileName ++ ".json")
        ) args
