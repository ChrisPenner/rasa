{-# LANGUAGE TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Buffer (
    Buffer(..)
    , cursor
    , text
  , Offset
  , Coord(..)
  , withCursor
  , inBuf
  , moveCursorBy
  , moveCursorBackBy
  , moveCursorTo
  , appendText
  , useCountFor
) where

import Data.Text as T
import Control.Monad.State (evalState, execState)
import Control.Arrow ((>>>))
import Control.Lens hiding (matching)

import TextLens
import Utils

type Offset = Int
type Coord = (Int, Int)

data Buffer c = Buffer {
    _text :: T.Text
  , _cursor :: c
} deriving (Show, Eq)
makeLenses ''Buffer

withCursor :: (Int -> Lens' T.Text T.Text) -> Lens' (Buffer Offset) T.Text
withCursor l = lens getter setter
    where getter = evalState $ do
            curs <- use cursor
            use (text.l curs)

          setter old new = flip execState old $ do
              curs <- use cursor
              (text.l curs) .= new

inBuf :: Lens' T.Text T.Text -> Lens' (Buffer a) T.Text
inBuf = (text.)


moveCursorBy :: Int -> Buffer Offset -> Buffer Offset
moveCursorBy n = do
    curs <- view cursor
    moveCursorTo (curs + n)

moveCursorTo :: Int -> Buffer Offset -> Buffer Offset
moveCursorTo n = execState $ do
    mx <- use (text.to T.length)
    curs <- use cursor
    cursor .= clamp 0 mx n

moveCursorBackBy :: Int -> Buffer Offset -> Buffer Offset
moveCursorBackBy = moveCursorBy . negate

appendText :: T.Text -> Buffer Offset -> Buffer Offset
appendText txt buf = (text.range curs curs .~ txt)
                        >>> moveCursorBy (T.length txt) $ buf
                            where curs = buf^.cursor

useCountFor :: Lens' (Buffer Offset) T.Text -> (Int -> Buffer Offset -> Buffer Offset) -> Buffer Offset -> Buffer Offset 
useCountFor l f = do
    count <- view $ l . to T.length
    f count

cursorIso :: Iso' (Buffer Offset) (Buffer Coord)
cursorIso = iso bufToCoord bufToOffset

bufToOffset :: Buffer Coord -> Buffer Offset
bufToOffset = do
    coord <- view cursor
    txt <- view text
    cursor .~ toOffset coord txt

bufToCoord :: Buffer Offset -> Buffer Coord
bufToCoord = do
    offs <- view cursor
    txt <- view text
    cursor .~ toCoord offs txt

toOffset :: Coord -> T.Text -> Offset
toOffset (row, col) t = rowCount + col
    where rowCount = t^.tillNextN row "\n" . to T.length

toCoord :: Offset -> T.Text -> Coord
toCoord offset = do
    row <- view $ before offset . matching "\n" . to T.length
    col <- view $ before offset . tillPrev "\n" . to T.length
    return (row, col)
