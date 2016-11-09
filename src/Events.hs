module Events (
                Event(..)
              , Continue(..)
              , handleEvent
              , nonEmpty
    ) where

import State

import qualified Data.Text as T
import Control.Lens
import Data.Char
import Data.List.Extra (dropEnd)

data Continue = Continue Event St

data Event =
    Append T.Text
  | Backspace
  | KillWord
  | SwitchBuf Int
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
someText = focusedBuf.nonEmpty

doEvent :: Event -> St -> St
doEvent (Append bufferText) =  focusedBuf %~ (`T.append` bufferText)
doEvent Backspace = someText %~ T.init
doEvent KillWord =  someText %~ (T.unwords . dropEnd 1 . T.words)

doEvent (SwitchBuf n) = do
    currentBuffer <- view focused
    numBuffers <- view (buffers.to length)
    let nextFocused = (n + currentBuffer) `mod` numBuffers
    set focused nextFocused

doEvent _ = id
