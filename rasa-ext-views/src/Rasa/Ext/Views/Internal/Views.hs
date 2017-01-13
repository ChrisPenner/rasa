{-# language FlexibleInstances, TemplateHaskell, DeriveFunctor #-}
module Rasa.Ext.Views.Internal.Views
  ( getViews
  , refocusView
  , rotate
  , splitRule
  , active
  , bufRef
  , closeBy
  , focusViewLeft
  , focusViewRight
  , focusViewAbove
  , focusViewBelow
  , windows
  , hSplit
  , vSplit
  , addSplit
  , getBufferViews
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
import Control.Monad.State
import Data.Default
import Data.Functor.Foldable

data SplitRule =
  Ratio Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

instance Default SplitRule where
  def = Ratio 0.5

data Dir = Hor
         | Vert
         deriving (Show)

instance Default Dir where
  def = Vert

data Split = Split
  { _dir :: Dir
  , _splitRule :: SplitRule
  } deriving (Show)
makeLenses ''Split

instance Default Split where
  def = Split def def

data View = View
  { _active :: Bool
  , _bufRef :: BufRef
  } deriving (Show)
makeLenses ''View

type Window = BiTree Split View

data Views = Views
  { _windows' :: Maybe Window
  }
makeLenses ''Views

windows :: HasEditor e => Lens' e (Maybe Window)
windows = ext.windows'

instance Show Views where
  show _ = "Views"

instance Default Views where
  def = Views Nothing

rotate :: Window -> Window
rotate = cata alg
  where alg (LeafF vw) = Leaf vw
        alg (BranchF sp s e) = Branch (sp & dir %~ rotDir) s e
        rotDir Hor = Vert
        rotDir Vert = Hor

splitView :: Dir -> Window -> Window
splitView d = cata alg
  where alg (LeafF vw) = if vw ^. active
                            then Branch (Split d def) (Leaf vw) (Leaf (vw & active .~ False))
                            else Leaf vw
        alg b = embed b

hSplit, vSplit :: Window -> Window
hSplit = splitView Hor
vSplit = splitView Vert

addSplit :: Dir -> BufRef -> Window -> Window
addSplit d bRef = Branch (def & dir .~ d) (Leaf $ View False bRef)

closeBy :: (View -> Bool) -> Window -> Maybe Window
closeBy p = zygo par alg
  where
    par (LeafF vw) = not $ p vw
    par (BranchF _ l r) = l || r
    alg (LeafF vw) = Just $ Leaf vw
    alg (BranchF sp (keepLeft, l) (keepRight, r))
      | keepLeft && keepRight = Branch sp <$> l <*> r
      | keepLeft = l
      | keepRight = r
      | otherwise = Nothing

focusViewLeft :: Window -> Window
focusViewLeft = ensureOneActive . zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Hor _) l r) = l || r
    par (BranchF (Split Vert _) l _) = l
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Hor _) (_, l) (_, r)) = Branch sp l r
    alg (BranchF sp@(Split Vert _) (_, l) (fromRight, r)) =
      Branch sp left r
        where left = if fromRight
                        then l & taking 1 (backwards traverse) . active .~ True
                        else l

focusViewRight :: Window -> Window
focusViewRight = ensureOneActive . zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Hor _) l r) = l || r
    par (BranchF (Split Vert _) _ r) = r
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Hor _) (_, l) (_, r)) = Branch sp l r
    alg (BranchF sp@(Split Vert _) (fromLeft, l) (_, r)) =
      Branch sp l right
        where right = if fromLeft
                         then r & taking 1 traverse . active .~ True
                         else r

focusViewAbove :: Window -> Window
focusViewAbove = ensureOneActive . zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Vert _) u d) = u || d
    par (BranchF (Split Hor _) u _) = u
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Vert _) (_, u) (_, d)) = Branch sp u d
    alg (BranchF sp@(Split Hor _) (_, u) (fromBottom, d)) =
      Branch sp top d
        where top = if fromBottom
                        then u & taking 1 (backwards traverse) . active .~ True
                        else u

focusViewBelow :: Window -> Window
focusViewBelow = ensureOneActive . zygo par alg
  where
    par (LeafF vw) = vw^.active
    par (BranchF (Split Vert _) u d) = u || d
    par (BranchF (Split Hor _) _ d) = d
    alg (LeafF vw) = Leaf (vw & active .~ False)
    alg (BranchF sp@(Split Vert _) (_, u) (_, d)) = Branch sp u d
    alg (BranchF sp@(Split Hor _) (fromTop, u) (_, d)) =
      Branch sp u bottom
        where bottom = if fromTop
                         then d & taking 1 traverse . active .~ True
                         else d

ensureOneActive :: Window -> Window
ensureOneActive w = if not $ anyOf traverse _active w
                       then w & taking 1 traverse . active .~ True
                       else w

refocusView :: Window -> Window
refocusView = taking 1 traverse . active .~ True

getViews :: Action Views
getViews = use ext

getBufferViews :: Action (Maybe (BiTree Split (View, Buffer)))
getBufferViews = do
  Views mWin <- getViews
  case mWin of
    Nothing -> return Nothing
    Just win -> sequence <$> mapM collect win
  where
    collect vw = do
      buf <- bufDo (vw^.bufRef) get
      return $ (,) vw <$> buf
