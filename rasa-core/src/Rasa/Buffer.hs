{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   StandaloneDeriving #-}

module Rasa.Buffer
  ( Buffer
  , Offset
  , Coord
  , Ext(..)
  , bufExts
  , attrs
  , text
  , newBuffer
  , useCountFor
  ) where

import qualified Data.Text as T
import Control.Lens hiding (matching)
import Data.Text.Lens (packed)
-- import Control.Lens.Text
import Data.Default
import Data.Dynamic
import Data.Map

import Rasa.Attributes

type Offset = Int

type Coord = (Int, Int)
data Ext = forall a. Show a => Ext a
deriving instance Show Ext

data Buffer c = Buffer
  { _text :: T.Text
  , _bufExts :: Map TypeRep Ext
  -- This list must always remain sorted by offset
  , _attrs :: [IAttr]
  }

makeLenses ''Buffer

instance Show (Buffer a) where
  show b = "<Buffer {text:" ++ show (b^..text.from packed.taking 30 traverse) ++ "...,\n"
           ++ "attrs: " ++ show (b^.attrs) ++ "\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

newBuffer :: T.Text -> Buffer Offset
newBuffer txt =
  Buffer
  { _text = txt
  , _bufExts = empty
  , _attrs = def
  }

-- withOffset :: (Int -> Lens' T.Text T.Text) -> Lens' (Buffer Offset) T.Text
-- withOffset l = lens getter setter
--   where
--     getter buf =
--       let curs = buf ^. cursor
--       in buf ^. text . l curs
--     setter old new =
--       let curs = old ^. cursor
--       in old & text . l curs .~ new

useCountFor :: Lens' (Buffer Offset) T.Text
            -> (Int -> Buffer Offset -> Buffer Offset)
            -> Buffer Offset
            -> Buffer Offset
useCountFor l f buf = f curs buf
  where
    curs = buf ^. l . to T.length

-- asCoord :: Iso' (Buffer Offset) (Buffer Coord)
-- asCoord = iso bufToCoord bufToOffset

-- bufToOffset :: Buffer Coord -> Buffer Offset
-- bufToOffset = do
--   coord <- view cursor
--   txt <- view text
--   cursor .~ toOffset coord txt

-- bufToCoord :: Buffer Offset -> Buffer Coord
-- bufToCoord = do
--   offs <- view cursor
--   txt <- view text
--   cursor .~ toCoord offs txt

-- toOffset :: Coord -> T.Text -> Offset
-- toOffset (row, col) t = clamp 0 (T.length t) $ rowCount + clamp 0 rowLen col
--   where
--     rowCount = t ^. intillNextN row "\n" . to T.length
--     rowLen = T.length $ T.lines t ^. ix row

-- toCoord :: Offset -> T.Text -> Coord
-- toCoord offset = do
--   row <- view $ before offset . matching "\n" . to T.length
--   col <-
--     case row of
--       0 -> return offset
--       _ -> view $ before offset . tillPrev "\n" . to T.length
--   return (row, col)

-- clamp :: Int -> Int -> Int -> Int
-- clamp mn mx n
--   | n < mn = mn
--   | n > mx = mx
--   | otherwise = n
