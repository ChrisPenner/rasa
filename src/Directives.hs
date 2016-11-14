{-# LANGUAGE OverloadedStrings #-}
module Directives (
                    Directive(..)
                  , toDirective
                  , Continue(..)
                  , handleEvent
                  ) where

import Events
import State
import Utils

import qualified Data.Text as T
import Control.Lens
import Control.Arrow ((>>>))
import Data.List.Extra (dropEnd)

data Continue = Continue [Directive] St

data Directive =
    Append T.Text
  | DeleteChar
  | KillWord
  | SwitchBuf Int
  | SwitchMode Mode
  | MoveCursor Int
  | StartOfLine
  | EndOfLine
  | StartOfBuffer
  | EndOfBuffer
  | FindNext T.Text
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

moveCursorBy :: Int -> Buffer -> Buffer
moveCursorBy n = do
    curs <- view cursor
    moveCursorTo (curs + n)

moveCursorTo :: Int -> Buffer -> Buffer
moveCursorTo n = do
    mx <- view (text.to T.length)
    curs <- view cursor
    set cursor (clamp 0 mx n)

deleteChar :: St -> St
deleteChar = do
    curs <- view (focusedBuf.cursor)
    st <- over (focusedBuf.text) (dropRange (curs-1) curs)
    return $ st & focusedBuf %~ moveCursorBy (-1)

spliceIn :: Int -> T.Text -> T.Text -> T.Text
spliceIn index txt existing = T.take index existing `mappend` txt `mappend` T.drop index existing

appendText :: T.Text -> Buffer -> Buffer
appendText txt buf = (text %~ spliceIn curs txt)
                        >>> moveCursorBy (T.length txt) $ buf
                            where curs = buf^.cursor

dropRange :: Int -> Int -> T.Text -> T.Text
dropRange start end = T.take start `mappend` T.drop end

findNext :: T.Text -> Buffer -> Buffer
findNext txt = do
    curs <- view cursor
    rest <- view $ text.to (T.drop curs)
    let offset = T.length . fst . T.breakOn txt $ rest
    moveCursorBy offset

doEvent :: Directive -> St -> St
doEvent (Append txt) =  focusedBuf %~ appendText txt
doEvent DeleteChar = deleteChar
doEvent KillWord =  someText %~ (T.unwords . dropEnd 1 . T.words)
doEvent (SwitchMode m) =  mode .~ m
doEvent (MoveCursor n) =  focusedBuf %~ moveCursorBy n
doEvent StartOfLine = focusedBuf %~ moveCursorTo 0
doEvent EndOfLine = focusedBuf %~ moveCursorTo 999999
doEvent StartOfBuffer = focusedBuf %~ moveCursorTo 0
doEvent EndOfBuffer = focusedBuf %~ moveCursorTo 999999
doEvent (FindNext txt) = focusedBuf %~ findNext txt

doEvent (SwitchBuf n) = do
    currentBuffer <- view focused
    numBuffers <- view (buffers.to length)
    let nextFocused = (n + currentBuffer) `mod` numBuffers
    set focused nextFocused

doEvent _ = id

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
toDirective Normal (Keypress 'X' _) = [DeleteChar]
toDirective Normal (Keypress 'x' _) = [MoveCursor 1, DeleteChar]
toDirective Normal (Keypress 'q' _) = [Exit]
toDirective Normal (Keypress 'c' [Ctrl]) = [Exit]

toDirective _ _ = []
