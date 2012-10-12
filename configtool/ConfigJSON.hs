module ConfigJSON where

import BasicTypes
import ConfigData 

jsObject :: [(String, ShowS)] -> ShowS
jsObject kvs =
  let f (k, showv) (rest, sep) =
        (showString (show k) .
        showString ": " .
        showv .
        showString sep .
        rest, ", ")
  in
  showString "{" .
  fst (foldr f (id, "") kvs) .
  showString "}"

class JSON a where
  toJSON :: a -> ShowS

instance JSON Authors where
  toJSON (Authors names aff) =
    let f name (rest, sep) =
          (showString "{\"name\": " .
          showString (show name) .
          showString ", \"affiliation\": " .
          showString (show aff) .
          showString "}" .
          showString sep .
          rest, ",")
    in 
    fst (foldr f (id,"") names)

instance JSON Talk where
  toJSON (Talk name start authors) =
    jsObject [("name", showString (show name))
             ,("start", toJSON start)
             ,("authors", toJSON authors)]

instance JSON a => JSON [a] where
  toJSON xs =
    let f x (rest, sep) =
          (toJSON x .
          showString sep .
          rest, ",")
    in
    showString "[" .
    fst (foldr f (id,"") xs) .
    showString "]"

instance JSON Session where
  toJSON (Session name mChair start talks) =
    showString "{" .
    showString "\"name\": " . showString (show name) .
    (maybe id (\chair -> showString ", \"chair\": " . showString (show chair)) mChair) .
    showString ", \"start\": " . toJSON start .
    showString ", \"talks\": " . toJSON talks .
    showString "}"

instance JSON Event where
  toJSON (Event name date sessions end) =
    jsObject [("name", showString (show name))
             ,("date", toJSON date)
             ,("end", toJSON end)
             ,("sessions", toJSON sessions)]

instance JSON Date where
  toJSON (Date m d y) =
    showChar '"' .
    shows m .
    showChar ' ' .
    showDigits 2 d .
    showString ", " .
    showDigits 4 y .
    showChar '"'
    
instance JSON Time where
  toJSON (Time h m) =
    showChar '"' .
    showDigits 2 h .
    showChar ':' .
    showDigits 2 m .
    showChar '"'

showDigits n m =
  let base = show m 
  in showString $ reverse $ take n $ (reverse base ++ repeat '0')
