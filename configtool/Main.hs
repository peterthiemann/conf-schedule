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
  flip mapM_ args (\fileName -> do
    putStrLn ("Parsing file " ++ fileName)
    evs <- runFile pFile fileName
    flip mapM_ (zip evs [0..]) (\(ev,i) -> do
      let outputFileName = fileName ++ extension i
          extension 0 = ".json"
          extension j = '-' : show j ++ ".json"
      writeFile outputFileName (T.unpack $ makeJson ev)
      putStrLn ("Writing output file " ++ outputFileName)
      ))
