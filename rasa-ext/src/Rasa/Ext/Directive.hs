module Rasa.Ext.Directive
  ( getFocusedBuffer
  , exit
  , overState
  , overBuffer
  , overText
  , insertText
  , deleteChar
  , killWord
  , switchBuf
  , moveCursor
  , moveCursorCoord
  , startOfLine
  , endOfLine
  , startOfBuffer
  , endOfBuffer
  , findNext
  , findPrev
  , deleteTillEOL
  , addBuffer
  ) where

import Rasa.Editor
import Control.Lens.Text
import Rasa.Buffer
import Rasa.Alteration

import Control.Lens
import qualified Data.Text as T
import Control.Monad.State
import Data.List.Extra (dropEnd)

addBuffer :: (String, T.Text) -> Alteration e ()
addBuffer info = zoom editor $ buffers %= (buffer info :)

getFocusedBuffer :: Alteration e (Buffer Offset)
getFocusedBuffer = zoom (editor . focusedBuf) get

embed :: (Editor -> Editor) -> Alteration e ()
embed = zoom editor . modify

exit :: Alteration e ()
exit = zoom editor $ exiting .= True

overState :: (Editor -> Editor) -> Alteration e ()
overState = embed

overBuffer :: (Buffer Offset -> Buffer Offset) -> Alteration e ()
overBuffer = embed . over focusedBuf

overText :: (T.Text -> T.Text) -> Alteration e ()
overText = embed . over (focusedBuf . text)

insertText :: T.Text -> Alteration e ()
insertText txt = embed $ focusedBuf %~ appendText txt

deleteChar :: Alteration e ()
deleteChar = embed $ focusedBuf %~ (withOffset before %~ T.dropEnd 1)

killWord :: Alteration e ()
killWord = embed $ focusedBuf . text %~ (T.unwords . dropEnd 1 . T.words)

switchBuf :: Int -> Alteration e ()
switchBuf n =
  embed $ execState $
  do currentBuffer <- use focused
     numBuffers <- use (buffers . to length)
     focused .= (n + currentBuffer) `mod` numBuffers

moveCursor :: Int -> Alteration e ()
moveCursor n = embed $ focusedBuf %~ moveCursorBy n

moveCursorCoord :: Coord -> Alteration e ()
moveCursorCoord crd = embed $ focusedBuf %~ moveCursorCoordBy crd

startOfLine :: Alteration e ()
startOfLine = embed $ focusedBuf %~ findPrev' "\n"

endOfLine :: Alteration e ()
endOfLine = embed $ focusedBuf %~ findNext' "\n"

startOfBuffer :: Alteration e ()
startOfBuffer = embed $ focusedBuf %~ moveCursorTo 0

endOfBuffer :: Alteration e ()
endOfBuffer = embed $ focusedBuf %~ useCountFor text moveCursorTo

findNext' :: T.Text -> Buffer Offset -> Buffer Offset
findNext' txt = useCountFor (withOffset after . tillNext txt) moveCursorBy

findNext :: T.Text -> Alteration e ()
findNext txt = embed $ focusedBuf %~ findNext' txt

findPrev' :: T.Text -> Buffer Offset -> Buffer Offset
findPrev' txt = useCountFor (withOffset before . tillPrev txt) moveCursorBackBy

findPrev :: T.Text -> Alteration e ()
findPrev txt = embed $ focusedBuf %~ findPrev' txt

deleteTillEOL' :: Buffer Offset -> Buffer Offset
deleteTillEOL' = withOffset after . tillNext "\n" .~ ""

deleteTillEOL :: Alteration e ()
deleteTillEOL = embed $ focusedBuf %~ deleteTillEOL'
