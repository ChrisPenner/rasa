{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Directives (applyDirectives) where

import State
import TextLens
import Buffer
import Types

import Control.Lens
import qualified Data.Text as T
import Control.Monad.State (execState)
import Data.Foldable (foldlM)
import Data.List.Extra (dropEnd)

applyDirectives :: St -> [Directive] -> IO St
applyDirectives = foldlM (flip doEvent)

deleteChar :: Buffer Offset -> Buffer Offset
deleteChar = withOffset before %~ T.dropEnd 1

findNext :: T.Text -> Buffer Offset -> Buffer Offset
findNext txt = useCountFor (withOffset after.tillNext txt) moveCursorBy

findPrev :: T.Text -> Buffer Offset -> Buffer Offset
findPrev txt = useCountFor (withOffset before.tillPrev txt) moveCursorBackBy

deleteTillEOL :: Buffer Offset -> Buffer Offset
deleteTillEOL = withOffset after.tillNext "\n" .~ ""

embed :: (St -> St) -> (St -> IO St)
embed = (return .)

doEvent :: Directive -> St -> IO St
doEvent (OverState f) = f
doEvent (OverBuffer f) = traverseOf focusedBuf f
doEvent (OverText f) = traverseOf (focusedBuf.text) f
doEvent (Append txt) = embed $ focusedBuf %~ appendText txt
doEvent DeleteChar = embed $ focusedBuf %~ deleteChar
doEvent KillWord = embed $ focusedBuf.text %~ (T.unwords . dropEnd 1 . T.words)
doEvent (MoveCursor n) = embed $  focusedBuf %~ moveCursorBy n
doEvent (MoveCursorCoordBy coords) = embed $  focusedBuf %~ moveCursorCoordBy coords
doEvent StartOfLine = embed $ focusedBuf %~ findPrev "\n"
doEvent EndOfLine = embed $ focusedBuf %~ findNext "\n"
doEvent StartOfBuffer = embed $ focusedBuf %~ moveCursorTo 0
doEvent EndOfBuffer = embed $ focusedBuf %~ useCountFor text moveCursorTo
doEvent (FindNext txt) = embed $ focusedBuf %~ findNext txt
doEvent (FindPrev txt) = embed $ focusedBuf %~ findPrev txt
doEvent DeleteTillEOL = embed $ focusedBuf %~ deleteTillEOL
doEvent Exit = return
doEvent Noop = return

doEvent (Effect io) = \st -> do
    io
    return st

doEvent (SwitchBuf n) = embed $ execState $ do
    currentBuffer <- use focused
    numBuffers <- use (buffers.to length)
    focused .= (n + currentBuffer) `mod` numBuffers
