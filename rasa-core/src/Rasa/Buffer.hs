{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   GeneralizedNewtypeDeriving, FlexibleInstances,
   StandaloneDeriving #-}

module Rasa.Buffer
  ( Buffer
  , BufAction
  , getBufAction
  , execBufAction
  , runBufAction
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

import Control.Monad.State

import Rasa.Attributes

type Coord = (Int, Int)
data Ext = forall a. Show a => Ext a
deriving instance Show Ext

data Buffer = Buffer
  { _text :: T.Text
  , _bufExts :: Map TypeRep Ext
  -- This list must always remain sorted by offset
  , _attrs :: [IAttr]
  }

makeLenses ''Buffer

instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text.from packed.taking 30 traverse) ++ "...,\n"
           ++ "attrs: " ++ show (b^.attrs) ++ "\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

newtype BufAction a = BufAction
  { getBufAction::StateT Buffer IO a
  } deriving (Functor, Applicative, Monad, MonadState Buffer, MonadIO)

execBufAction :: Buffer -> BufAction a -> IO Buffer
execBufAction buf = flip execStateT buf . getBufAction

runBufAction :: Buffer -> BufAction a -> IO (a, Buffer)
runBufAction buf = flip runStateT buf . getBufAction

newBuffer :: T.Text -> Buffer
newBuffer txt =
  Buffer
  { _text = txt
  , _bufExts = empty
  , _attrs = def
  }

-- withOffset :: (Int -> Lens' T.Text T.Text) -> Lens' (Buffer ) T.Text
-- withOffset l = lens getter setter
--   where
--     getter buf =
--       let curs = buf ^. cursor
--       in buf ^. text . l curs
--     setter old new =
--       let curs = old ^. cursor
--       in old & text . l curs .~ new

useCountFor :: Lens' Buffer  T.Text
            -> (Int -> Buffer  -> Buffer )
            -> Buffer 
            -> Buffer 
useCountFor l f buf = f curs buf
  where
    curs = buf ^. l . to T.length

-- clamp :: Int -> Int -> Int -> Int
-- clamp mn mx n
--   | n < mn = mn
--   | n > mx = mx
--   | otherwise = n
