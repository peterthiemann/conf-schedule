{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, Rank2Types #-}

module ParseConfigUU where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils hiding (lexeme)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import ParseUtils
import BasicTypes
import ConfigData
import ConfigJSON

pFile :: Parser Event
pFile = pMany (commentLine <|> emptyLine) *> pEvent

pEvent :: Parser Event
pEvent = Event <$> eventLine <*> dateLine <*> roomLine <*> pSessions <*> endLine

pSessions :: Parser [Session]
pSessions = pSome pSession

pSession :: Parser Session
pSession = Session <$> sessionLine <*> pMaybe chairLine <*> startLine <*> pTalks

pTalks :: Parser [Talk]
pTalks = pMany (Talk <$> talkLine <*> startLine <*> pAuthors)

pAuthors :: Parser [Authors]
pAuthors = pSome (Authors <$> pSome authorLine <*> affiliationLine)

-- middle level definitions

eventLine :: Parser String
eventLine = keyLine "Event"

dateLine :: Parser Date
dateLine = kvLine "Date" pDate

roomLine :: Parser String
roomLine = keyLine "Room"

sessionLine :: Parser String
sessionLine = keyLine "Session"

startLine :: Parser Time
startLine = kvLine "Start" pTime

authorLine :: Parser String
authorLine = keyLine "Author"

affiliationLine :: Parser String
affiliationLine = keyLine "Affiliation"

talkLine :: Parser String
talkLine = keyLine "Talk"

chairLine :: Parser String
chairLine = keyLine "Chair"

endLine :: Parser Time
endLine = kvLine "End" pTime

-- low level definitions

keyLine :: String -> Parser String
keyLine kw = 
  kvLine kw (pMunch (/= '\n'))

kvLine :: String -> ParserTrafo a a
kvLine kw p =
  (lexeme (pToken kw) *> lexeme (pSym ':') *> p <* pLF)
  <* pMany (commentLine <|> emptyLine)

-- commentLine :: Parser String
commentLine =
  inlineSpaces *> pToken "--" *> pMunch (/= '\n') <* pLF

-- emptyLine :: Parser String
emptyLine =
  inlineSpaces <* pLF

ex0 = "Event: ICFP day #1\n"
ex1 = "Event: ICFP day #1\nDate: Sep 10, 2012\n"
ex2 = "-- a comment line\n"
ex3 = "   -- a comment line\n"

