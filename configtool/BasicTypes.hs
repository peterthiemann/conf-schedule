module BasicTypes where

data Month =
  Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec 
      deriving (Enum, Bounded, Eq, Show, Ord)

data Date =
  Date Month Int Int
  deriving (Show)

data Time =
  Time Int Int
  deriving (Show)

