{-# language DeriveFunctor, TemplateHaskell, DeriveTraversable #-}
module Rasa.Ext.Views.Internal.Types
  ( windows
  , active
  , scrollPos
  , bufIndex
  , hSplit
  , vSplit
  , closeView
  , SplitDir(..)
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
import Control.Applicative
import Data.Maybe

data SplitRule =
  Ratio Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data SplitInfo = SplitInfo
  { splitRule :: SplitRule
  } deriving (Show)

instance Default SplitInfo where
  def = SplitInfo (Ratio 0.5)

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

instance Default View where
  def = View
    { _active=True
    , _scrollPos'=ScrollPos 0
    , _bufIndex=0
    }

scrollPos :: Lens' View Int
scrollPos = scrollPos'.scrollAmt

data SplitDir = Hor
         | Vert
         deriving (Show)

data Window a =
  Split SplitDir SplitInfo (Window a) (Window a)
    | Single a
    deriving (Show, Functor, Foldable, Traversable)

data Views = Views
  { _windows' :: Window View
  } deriving Show
makeLenses ''Views

windows :: HasEditor e => Lens' e (Window View)
windows = ext.windows'

instance Default Views where
  def = Views $ Single def

splitDir :: SplitDir -> Window View -> Window View
splitDir d (Split dir info start end) = Split dir info (splitDir d start) (splitDir d end)
splitDir d (Single vw) = if vw^.active
                            then Split d def (Single vw) (Single vw)
                            else Single vw

hSplit :: Window View -> Window View
hSplit = splitDir Hor

vSplit :: Window View -> Window View
vSplit = splitDir Vert

closeView :: Window View -> Window View
closeView w = fromMaybe (Single def) $ closeViewM w

closeViewM :: Window View -> Maybe (Window View)
closeViewM (Single v) = if v^.active then Nothing
                                    else Just $ Single v

closeViewM (Split dir info start end) =
  (Split dir info <$> s <*> e) <|> s <|> e
  where s = closeViewM start
        e = closeViewM end

