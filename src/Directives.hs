{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Directives (
                    Directive(..)
                  , toDirective
                  , Continue(..)
                  , handleEvent
                  ) where

import Events
import State
import TextLens
import Buffer

import qualified Data.Text as T
import Control.Lens
import Control.Monad.State (execState)
import Data.List.Extra (dropEnd)

data Continue = Continue [Directive] St

data Directive =
    Append T.Text
  | DeleteChar
  | KillWord
  | SwitchBuf Int
  | SwitchMode Mode
  | MoveCursor Int
  | MoveCursorCoordBy Coord
  | StartOfLine
  | EndOfLine
  | StartOfBuffer
  | EndOfBuffer
  | FindNext T.Text
  | FindPrev T.Text
  | Noop
  | Exit
  deriving (Show, Eq)

handleEvent :: Continue -> Continue
handleEvent (Continue dirs st) = Continue dirs $ foldl (flip doEvent) st dirs

nonEmpty :: Prism' T.Text T.Text
nonEmpty = prism id $ \t ->
    if T.null t
       then Left t
       else Right t

someText :: (T.Text -> Identity T.Text) -> St -> Identity St
someText = focusedBuf.text.nonEmpty

deleteChar :: St -> St
deleteChar = execState $ do
    curs <- use (focusedBuf.cursor)
    focusedBuf.text.range (curs-1) curs .= ""
    focusedBuf %= moveCursorBy (-1)

findNext :: T.Text -> Buffer Offset -> Buffer Offset
findNext txt = useCountFor (withCursor after.tillNext txt) moveCursorBy

findPrev :: T.Text -> Buffer Offset -> Buffer Offset
findPrev txt = useCountFor (withCursor before.tillPrev txt) moveCursorBackBy

doEvent :: Directive -> St -> St
doEvent (Append txt) =  focusedBuf %~ appendText txt
doEvent DeleteChar = deleteChar
doEvent KillWord =  someText %~ (T.unwords . dropEnd 1 . T.words)
doEvent (SwitchMode m) =  mode .~ m
doEvent (MoveCursor n) =  focusedBuf %~ moveCursorBy n
doEvent (MoveCursorCoordBy coords) =  focusedBuf %~ moveCursorCoordBy coords
doEvent StartOfLine = focusedBuf %~ findPrev "\n"
doEvent EndOfLine = focusedBuf %~ findNext "\n"
doEvent StartOfBuffer = focusedBuf %~ moveCursorTo 0
doEvent EndOfBuffer = focusedBuf %~ useCountFor text moveCursorTo
doEvent (FindNext txt) = focusedBuf %~ findNext txt
doEvent (FindPrev txt) = focusedBuf %~ findPrev txt

doEvent (SwitchBuf n) = execState $ do
    currentBuffer <- use focused
    numBuffers <- use (buffers.to length)
    focused .= (n + currentBuffer) `mod` numBuffers

toDirective :: Mode -> Event -> [Directive]
toDirective Insert Esc = [SwitchMode Normal]
toDirective Insert BS = [DeleteChar]
toDirective Insert Enter = [Append "\n"]
toDirective Insert (Keypress 'w' [Ctrl]) = [KillWord]
toDirective Insert (Keypress 'c' [Ctrl]) = [Exit]
toDirective Insert (Keypress c mods) = [Append (T.singleton c)]

toDirective Normal (Keypress 'i' _ )  = [SwitchMode Insert]
toDirective Normal (Keypress 'I' _ )  = [SwitchMode Insert, StartOfLine]
toDirective Normal (Keypress 'a' _ )  = [SwitchMode Insert, MoveCursor 1]
toDirective Normal (Keypress 'A' _ )  = [SwitchMode Insert, EndOfLine]
toDirective Normal (Keypress '0' _ )  = [StartOfLine]
toDirective Normal (Keypress '$' _ )  = [FindNext "\n"]
toDirective Normal (Keypress 'g' _ )  = [StartOfBuffer]
toDirective Normal (Keypress 'G' _ )  = [EndOfBuffer]
toDirective Normal (Keypress 'o' _ )  = [SwitchMode Insert, EndOfLine, Append "\n"]
toDirective Normal (Keypress 'O' _ )  = [SwitchMode Insert, StartOfLine, Append "\n"]
toDirective Normal (Keypress '+' _ ) = [SwitchBuf 1]
toDirective Normal (Keypress '-' _ ) = [SwitchBuf (-1)]
toDirective Normal (Keypress 'h' _ )  = [MoveCursor (-1)]
toDirective Normal (Keypress 'l' _ )  = [MoveCursor 1]
toDirective Normal (Keypress 'k' _ )  = [MoveCursorCoordBy (-1, 0)]
toDirective Normal (Keypress 'j' _ )  = [MoveCursorCoordBy (1, 0)]
toDirective Normal (Keypress 'f' _ )  = [FindNext "f"]
toDirective Normal (Keypress 'F' _ )  = [FindPrev "f"]
toDirective Normal (Keypress 'X' _) = [DeleteChar]
toDirective Normal (Keypress 'x' _) = [MoveCursor 1, DeleteChar]
toDirective Normal (Keypress 'q' _) = [Exit]
toDirective Normal (Keypress 'c' [Ctrl]) = [Exit]

toDirective _ _ = []
