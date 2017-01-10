{-# language FlexibleInstances, TemplateHaskell #-}
module Rasa.Ext.Views
  (
  getViews
  , rotate
  , bufIndex
  , splitRule
  , active
  , closeBy
  , moveRight
  , Dir(..)
  , SplitRule(..)
  , Window
  , Split(..)
  , Views(..)
  , View(..)
  , BiTree(..)
  , BiTreeF(..)
  ) where

import Rasa.Ext
import Rasa.Ext.Views.Internal.BiTree

import Control.Lens
import Data.Default
import Data.Functor.Foldable

data SplitRule =
  Percentage Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

data Dir = Hor
         | Vert
         deriving (Show)

data Split = Split
  { _dir :: Dir
  , _splitRule :: SplitRule
  } deriving (Show)

makeLenses ''Split

data View = View
  { _active :: Bool
  , _bufIndex :: Int
  } deriving (Show)

makeLenses ''View

instance Default View where
  def = View True 0

branch :: Split -> Window -> Window -> Window
branch = Branch

leaf :: View -> Window
leaf = Leaf

type Window = BiTree Split View

data Views = Views
  { main :: Window
  }

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views $ branch (Split Vert $ Percentage 0.5)
                              (leaf $ View True 0)
                              $ branch (Split Hor $ Percentage 0.5)
                                  (leaf $ View False 1)
                                  (leaf $ View False 1)

rotate :: Window -> Window
rotate = cata alg
  where alg (LeafF vw) = Leaf vw
        alg (BranchF sp s e) = Branch (sp & dir %~ rotDir) s e
        rotDir Hor = Vert
        rotDir Vert = Hor

closeBy :: (View -> Bool) -> Window -> Window
closeBy p = zygo par alg
  where
    par (LeafF vw) = not $ p vw
    par (BranchF _ l r) = l || r
    alg (LeafF vw) = Leaf vw
    alg (BranchF sp (keepLeft, al) (keepRight, ar))
      | keepLeft && keepRight = Branch sp al ar
      | keepLeft = al
      | keepRight = ar
      | otherwise = Leaf def

moveRight :: Window -> Window
moveRight = zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Hor _) l r) = l || r
    par (BranchF (Split Vert _) _ r) = r
    alg (LeafF vw) = Leaf vw
    alg (BranchF sp@(Split Hor _) (_, al) (_, ar)) = Branch sp al ar
    alg (BranchF sp@(Split Vert _) (fromLeft, al) (_, ar)) =
      Branch sp al right
        where right = if fromLeft
                         then ar & taking 1 traverse . active .~ True
                         else ar

getViews :: Action Views
getViews = use ext
