module Events (
                Event(..)
              , Continue(..)
              , handleEvent
    ) where

import State (St, text)

import qualified Data.Text as T
import Control.Lens
import Data.Char

data Continue = Continue Event St

data Event =
    Append T.Text
  | Backspace
  | KillWord
  | Noop
  | Exit


handleEvent :: Continue -> Continue
handleEvent (Continue evt st) =
    case evt of
      Exit -> Continue Exit st
      e -> Continue e $ doEvent e st


doEvent :: Event -> St -> St
doEvent (Append bufferText) =  text %~ (`T.append` bufferText)
doEvent Backspace =  text %~ T.init
doEvent KillWord =  text %~ d . T.stripEnd
    where d t
            | isAlphaNum . T.last $ t = T.dropWhileEnd isAlphaNum t
            | otherwise = T.init t
doEvent _ = id
