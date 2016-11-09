{-# LANGUAGE OverloadedStrings #-}
module Directives (
                    Directive(..)
                  , toDirective
                  , Continue(..)
                  , handleEvent
                  ) where

import Events
import State

import qualified Data.Text as T
import Control.Lens
import Data.List.Extra (dropEnd)

data Continue = Continue Directive St

data Directive =
    Append T.Text
  | DeleteChar
  | KillWord
  | SwitchBuf Int
  | Noop
  | Exit

toDirective :: Event -> St -> Directive
toDirective e st = case st^.mode of 
                     Insert -> insertMode e st
                     _ -> Noop

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
doEvent :: Directive -> St -> St
doEvent (Append bufferText) =  focusedBuf %~ (`T.append` bufferText)
doEvent DeleteChar = someText %~ T.init
doEvent KillWord =  someText %~ (T.unwords . dropEnd 1 . T.words)

doEvent (SwitchBuf n) = do
    currentBuffer <- view focused
    numBuffers <- view (buffers.to length)
    let nextFocused = (n + currentBuffer) `mod` numBuffers
    set focused nextFocused

doEvent _ = id




insertMode :: Event -> St -> Directive
insertMode Esc _ = Exit
insertMode BS _ = DeleteChar
insertMode Enter _ = Append "\n"
insertMode (Keypress 'w' [Ctrl]) _ = KillWord
insertMode (Keypress '+' _ ) _ = SwitchBuf 1
insertMode (Keypress '-' _ ) _ = SwitchBuf (-1)
insertMode (Keypress c mods) _ = Append (T.singleton c)


