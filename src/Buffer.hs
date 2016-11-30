{-# LANGUAGE Rank2Types #-}
module Buffer (
    Buffer
  , Offset
  , Coord
  , cursor
  , text
  , buffer
  , filename

  , withOffset
  , moveCursorBy
  , moveCursorCoordBy
  , moveCursorBackBy
  , moveCursorTo
  , appendText
  , useCountFor
  , asCoord
) where

import qualified Data.Text as T
import Control.Lens hiding (matching)

import TextLens
import Utils

type Offset = Int
type Coord = (Int, Int)

data Buffer c = Buffer {
    _text :: T.Text
  , _cursor :: c
  , _filename :: String
} deriving (Show, Eq)

makeLenses ''Buffer

buffer :: (String, T.Text) -> Buffer Offset
buffer (fname, t) = Buffer {
        _text=t
      , _cursor=0
      , _filename=fname
}

withOffset :: (Int -> Lens' T.Text T.Text) -> Lens' (Buffer Offset) T.Text
withOffset l = lens getter setter
    where getter buf = let curs = buf^.cursor
                        in buf^.text.l curs

          setter old new = let curs = old^.cursor
                            in old & text.l curs .~ new

moveCursorBy :: Int -> Buffer Offset -> Buffer Offset
moveCursorBy n = do
    curs <- view cursor
    moveCursorTo (curs + n)

moveCursorCoordBy :: Coord -> Buffer Offset -> Buffer Offset
moveCursorCoordBy c = asCoord.cursor %~ addPair c
    where addPair (a, b) (a', b') = (a + a', b + b')

moveCursorTo :: Int -> Buffer Offset -> Buffer Offset
moveCursorTo n = do
    mx <- view (text.to T.length)
    cursor .~ clamp 0 mx n

moveCursorBackBy :: Int -> Buffer Offset -> Buffer Offset
moveCursorBackBy = moveCursorBy . negate

appendText :: T.Text -> Buffer Offset -> Buffer Offset
appendText txt buf = buf
                     & text.range curs curs .~ txt
                     & moveCursorBy (T.length txt)
                         where curs = buf^.cursor

useCountFor :: Lens' (Buffer Offset) T.Text -> (Int -> Buffer Offset -> Buffer Offset) -> Buffer Offset -> Buffer Offset
useCountFor l f buf = f curs buf
    where curs = buf^.l.to T.length

asCoord :: Iso' (Buffer Offset) (Buffer Coord)
asCoord = iso bufToCoord bufToOffset

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
toOffset (row, col) t = clamp 0 (T.length t) $ rowCount + clamp 0 rowLen col
    where rowCount = t^.intillNextN row "\n" . to T.length
          rowLen = T.length $ T.lines t ^. ix row

toCoord :: Offset -> T.Text -> Coord
toCoord offset = do
    row <- view $ before offset . matching "\n" . to T.length
    col <- case row of
             0 -> return offset
             _ -> view $ before offset . tillPrev "\n" . to T.length
    return (row, col)
