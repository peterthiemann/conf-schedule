{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction #-}

module ParseUtils where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils hiding (lexeme)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import BasicTypes

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
pDayNum = lexeme pNaturalRaw

pYearNum :: Parser Int
pYearNum = lexeme pNaturalRaw

pTime :: Parser Time
pTime = Time <$> pHourNum <* lexeme (pSym ':') <*> pMinuteNum

pHourNum = lexeme pNaturalRaw
pMinuteNum = lexeme pNaturalRaw

-- | Testing
run :: Show t => Parser t -> String -> String
run p inp = 
  let r = parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
  in  show r

runFile :: Show t => Parser t -> String -> IO t
runFile p fname =
  do inp <- readFile fname
     let (r,msgs) = parse ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
     putStrLn $ show r
     putStrLn ""
     mapM_ (putStrLn . show) msgs
     return r

dex1 = "Sep 9, 2012"
dex2 = "Oct 10, 2012"
