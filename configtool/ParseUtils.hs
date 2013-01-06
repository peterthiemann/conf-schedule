{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction #-}

module ParseUtils where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils hiding (lexeme)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import BasicTypes
import Data.Char

type Parser a = P (Str Char String LineColPos) a

-- inlineSpaces :: Parser String
inlineSpaces = pMunch (`elem` " \t")

-- | A lexeme eats trailing *inline* whitespace
lexeme :: ParserTrafo a a
lexeme p = p <* inlineSpaces

-- | Parse a date in RFC-like format, e.g., "Sep 9, 2012"
pDate :: Parser Date 
pDate = Date <$> pMonth <*> pDayNum <* lexeme (pSym ',') <*> pYearNum

-- | Parse a month
pMonth :: Parser Month
pMonth = lexeme pEnumRaw

pDayNum :: Parser Int
pDayNum = lexeme p1_31Raw

pYearNum :: Parser Int
pYearNum = lexeme (pNDigitInt 4) <?> "year"

pTime :: Parser Time
pTime = Time <$> pHourNum <* lexeme (pSym ':') <*> pMinuteNum

pHourNum = lexeme p00_23Raw
pMinuteNum = lexeme p00_59Raw

twoDigitNum d1 d2 = 10*d1 + d2

p1_31Raw =
  twoDigitNum <$> pDigitVal '0' <*> pDigitRange '1' '9'
  <|> twoDigitNum <$> pDigitRange '1' '2' <*> pDigitVal '9'
  <|> twoDigitNum <$> pDigitRange '3' '3' <*> pDigitVal '1'
  <|> pDigitRange '1' '9'
  <?> "1-31"

p00_23Raw =
  twoDigitNum <$> pDigitVal '1' <*> pDigitVal '9'
  <|> twoDigitNum <$> pDigitRange '2' '2' <*> pDigitVal '3'
  <|> pDigitVal '9'
  <?> "0-23"

p00_59Raw =
  (twoDigitNum <$> pDigitVal '5' <*> pDigitVal '9') <?> "00-59"

pDigitVal h = 
  pDigitRange '0' h
pDigitRange l h =
  (\c -> ord c - ord '0') <$>
  (pSatisfy (\c -> l <= c && c <= h) (Insertion "zero" l 5))

pNDigitInt n = (read :: String -> Int) <$> pExact n pDigit

-- | Testing
run :: Show t => Parser t -> String -> String
run p inp = 
  let r = parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
  in  show r

demoFile :: Show t => Parser t -> String -> IO t
demoFile p fname =
  do out <- runFile p fname
     putStrLn ""
     putStrLn $ show out
     return out

runFile :: Show t => Parser t -> String -> IO t
runFile p fname =
  do inp <- readFile fname
     let (r,msgs) = parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
     mapM_ (putStrLn . show) msgs
     return r

dex1 = "Sep 9, 2012"
dex2 = "Oct 10, 2012"
dex3 = "October 0, 2012"
dex4 = "March 22, 101010" -- gets March 2 :-(
dex5 = "Mar 22, 101010" -- gets March 22
