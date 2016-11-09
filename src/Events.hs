module Events (
                Event(..)
              , Continue(..)
              , handleEvent
              , nonEmpty
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

nonEmpty :: Prism' T.Text T.Text
nonEmpty = prism id $ \t ->
    if T.null t
       then Left t
       else Right t

someText :: (T.Text -> Identity T.Text) -> St -> Identity St
someText = text.nonEmpty

doEvent :: Event -> St -> St
doEvent (Append bufferText) =  text %~ (`T.append` bufferText)
doEvent Backspace = someText %~ T.init
doEvent KillWord =  someText %~ d . T.stripEnd
    where d t
            | isAlphaNum . T.last $ t = T.dropWhileEnd isAlphaNum t
            | otherwise = T.init t
doEvent _ = id
