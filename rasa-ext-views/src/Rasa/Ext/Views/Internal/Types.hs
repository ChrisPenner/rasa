{-# language DeriveFunctor, TemplateHaskell, DeriveTraversable #-}
module Rasa.Ext.Views.Internal.Types
  ( windows
  , active
  , scrollPos
  , bufIndex
  , Dir(..)
  , SplitRule(..)
  , Views(..)
  , Window(..)
  , View(..)
  , SplitInfo(..)
  , ScrollPos(..)
  ) where

import Rasa.Ext
import Control.Lens

import Data.Default

data SplitRule =
  Ratio Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data SplitInfo = SplitInfo
  { splitRule :: SplitRule
  } deriving (Show)

newtype ScrollPos = ScrollPos
  { _scrollAmt :: Int
  } deriving (Show)

makeLenses ''ScrollPos

data View = View
  { _active :: Bool
  , _scrollPos' :: ScrollPos
  , _bufIndex :: Int
  } deriving (Show)

makeLenses ''View

scrollPos :: Lens' View Int
scrollPos = scrollPos'.scrollAmt

data Dir = Hor
         | Vert
         deriving (Show)

data Window a =
  Split Dir SplitInfo (Window a) (Window a)
    | Single a
    deriving (Show, Functor, Foldable, Traversable)

data Views = Views
  { _windows' :: Window View
  } deriving Show
makeLenses ''Views

windows :: HasEditor e => Lens' e (Window View)
windows = ext.windows'

instance Default Views where
  def = Views $ Split Hor (SplitInfo $ Ratio 0.5)
                              (Single $ View True (ScrollPos 0) 0)
                              $ Split Vert (SplitInfo $ Ratio 0.5)
                                  (Single $ View False (ScrollPos 0) 0)
                                  (Single $ View True (ScrollPos 0) 0)
