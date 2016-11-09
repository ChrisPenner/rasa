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
  | SwitchMode Mode
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

doEvent :: Directive -> St -> St
doEvent (Append bufferText) =  focusedBuf %~ (`T.append` bufferText)
doEvent DeleteChar = someText %~ T.init
doEvent KillWord =  someText %~ (T.unwords . dropEnd 1 . T.words)
doEvent (SwitchMode m) =  mode .~ m

doEvent (SwitchBuf n) = do
    currentBuffer <- view focused
    numBuffers <- view (buffers.to length)
    let nextFocused = (n + currentBuffer) `mod` numBuffers
    set focused nextFocused

doEvent _ = id

toDirective :: Mode -> Event -> Directive
toDirective _ (Keypress '1' _ )  = SwitchMode Insert
toDirective _ (Keypress '2' _ ) = SwitchMode Normal
toDirective _ Esc = Exit

toDirective Insert BS = DeleteChar
toDirective Insert Enter = Append "\n"
toDirective Insert (Keypress 'w' [Ctrl]) = KillWord
toDirective Insert (Keypress '+' _ ) = SwitchBuf 1
toDirective Insert (Keypress '-' _ ) = SwitchBuf (-1)
toDirective Insert (Keypress c mods) = Append (T.singleton c)

toDirective Normal (Keypress 'X' _) = DeleteChar

toDirective _ _ = Noop
