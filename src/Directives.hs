{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Directives (applyDirectives) where

import State
import TextLens
import Buffer
import Types

import Control.Lens
import qualified Data.Text as T
import Control.Monad.State (execState)
import Data.List.Extra (dropEnd)

applyDirectives :: Continue -> Continue
applyDirectives (Continue st dirs) = Continue newState dirs
    where newState = foldl (flip doEvent) st dirs

deleteChar :: Buffer Offset -> Buffer Offset
deleteChar = withOffset before %~ T.dropEnd 1

findNext :: T.Text -> Buffer Offset -> Buffer Offset
findNext txt = useCountFor (withOffset after.tillNext txt) moveCursorBy

findPrev :: T.Text -> Buffer Offset -> Buffer Offset
findPrev txt = useCountFor (withOffset before.tillPrev txt) moveCursorBackBy

deleteTillEOL :: Buffer Offset -> Buffer Offset
deleteTillEOL = withOffset after.tillNext "\n" .~ ""

doEvent :: Directive -> St -> St
doEvent (CustomOp (OverState f)) = f
doEvent (CustomOp (OverBuffer f)) = focusedBuf %~ f
doEvent (CustomOp (OverText f)) = focusedBuf.text %~ f
doEvent (Append txt) =  focusedBuf %~ appendText txt
doEvent DeleteChar = focusedBuf %~ deleteChar
doEvent KillWord =  focusedBuf.text %~ (T.unwords . dropEnd 1 . T.words)
doEvent (MoveCursor n) =  focusedBuf %~ moveCursorBy n
doEvent (MoveCursorCoordBy coords) =  focusedBuf %~ moveCursorCoordBy coords
doEvent StartOfLine = focusedBuf %~ findPrev "\n"
doEvent EndOfLine = focusedBuf %~ findNext "\n"
doEvent StartOfBuffer = focusedBuf %~ moveCursorTo 0
doEvent EndOfBuffer = focusedBuf %~ useCountFor text moveCursorTo
doEvent (FindNext txt) = focusedBuf %~ findNext txt
doEvent (FindPrev txt) = focusedBuf %~ findPrev txt
doEvent DeleteTillEOL = focusedBuf %~ deleteTillEOL
doEvent Exit = id
doEvent Noop = id

doEvent (SwitchBuf n) = execState $ do
    currentBuffer <- use focused
    numBuffers <- use (buffers.to length)
    focused .= (n + currentBuffer) `mod` numBuffers

