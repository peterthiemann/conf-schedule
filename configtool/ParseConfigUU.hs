{-# LANGUAGE  FlexibleContexts, NoMonomorphismRestriction, Rank2Types #-}

module ParseConfigUU where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils hiding (lexeme)
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)

import ParseUtils
import BasicTypes
import ConfigData
--import ConfigJSON

pFile :: Parser Event
pFile = pMany (commentLine <|> emptyLine) *> pEvent

pEvent :: Parser Event
pEvent = Event <$> eventLine <*> dateLine <*> roomLine <*> pSessions <*> endLine

pSessions :: Parser [Session]
pSessions = pSome pSession

pSession :: Parser Session
pSession = Session <$> sessionLine <*> pMaybe chairLine <*> pMaybe roomLine <*> startLine <*> pTalks

pTalks :: Parser [Talk]
pTalks = pMany (Talk <$> talkLine <*> startLine <*> pAuthors)

pAuthors :: Parser [Authors]
pAuthors = pSome (Authors <$> pSome pAuthor <*> pMaybe affiliationLine)

pAuthor :: Parser Author
pAuthor = Author <$> authorLine <*> pMaybe urlLine

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

urlLine :: Parser String
urlLine = keyLine "URL"

affiliationLine :: Parser String
affiliationLine = keyLine "Affiliation"

talkLine :: Parser String
talkLine = keyLine "Talk"

chairLine :: Parser String
chairLine = keyLine "Chair"

endLine :: Parser Time
endLine = kvLine "End" pTime

-- low level definitions

munchToEOL :: Parser String
munchToEOL =
  pMunch (not . (`elem` "\n\r"))

pEOL :: Parser Char
pEOL =
  pLF <|> (pCR *> pLF)

keyLine :: String -> Parser String
keyLine kw = 
  kvLine kw munchToEOL

-- kvLine :: String -> ParserTrafo a a
kvLine kw p =
  (lexeme (pToken kw) *> lexeme (pSym ':') *> p <* pEOL)
  <* pMany (commentLine <|> emptyLine)

commentLine :: Parser String
commentLine =
  inlineSpaces *> pSym '-' *> pSym '-' *> pMany (pSym '-') *> munchToEOL <* pEOL

emptyLine :: Parser String
emptyLine =
  inlineSpaces <* pEOL

ex0 = "Event: ICFP day #1\n"
ex1 = "Event: ICFP day #1\nDate: Sep 10, 2012\n"
ex2 = "-- a comment line\n"
ex3 = "   -- a comment line\n"

