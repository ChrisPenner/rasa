{-# language FlexibleInstances, TemplateHaskell, DeriveFunctor #-}
module Rasa.Ext.Views.Internal.Views
  ( rotate
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
import Data.Default
import Data.Functor.Foldable

-- | A 'SplitRule' determines size of each half of the split.
--
-- - @Ratio Double@ sets the split to the given ratio; the double must be
-- between 0 and 1; for example a value of @0.25@ sets the first portion of the
-- split to 1/4 of the available space; the other portion takes the remaining
-- 3/4 of the space
--
-- - @FromStart Int@ makes the first half of the split (top/left respectively)
-- the set number of rows or columns respectively, the other half of the split
-- gets the rest.
--
-- - @FromEnd Int@ makes the first half of the split (top/left respectively)
-- the set number of rows or columns respectively, the other half of the split
-- gets the rest.

data SplitRule =
  Ratio Double
  | FromStart Int
  | FromEnd Int
  deriving (Show)

instance Default SplitRule where
  def = Ratio 0.5

-- | - 'Hor' denotes a horizontal split.
-- - 'Vert' denotes a vertical split.
data Dir = Hor
         | Vert
         deriving (Show)

instance Default Dir where
  def = Vert

-- | A Split contains info about a the direction and allocation of a split branch.
data Split = Split
  { _dir :: Dir
  , _splitRule :: SplitRule
  } deriving (Show)
makeLenses ''Split

instance Default Split where
  def = Split def def

-- | A 'View' contains info about a viewport; Whether it's selected and which buffer should be displayed.
data View = View
  { _active :: Bool
  , _bufRef :: BufRef
  } deriving (Show)
makeLenses ''View

-- | A tree of windows branched with splits.
type Window = BiTree Split View

-- | Extension state storing the window layout
data Views = Views
  { _windows' :: Maybe Window
  } deriving (Show)
makeLenses ''Views

-- | A lens to access the stored windows
windows :: HasEditor e => Lens' e (Maybe Window)
windows = ext.windows'

instance Default Views where
  def = Views Nothing

-- | Flip all Horizontal splits to Vertical ones and vice versa.
rotate :: Window -> Window
rotate = cata alg
  where alg (LeafF vw) = Leaf vw
        alg (BranchF sp s e) = Branch (sp & dir %~ rotDir) s e
        rotDir Hor = Vert
        rotDir Vert = Hor

-- | Split active views in the given direction
splitView :: Dir -> Window -> Window
splitView d = cata alg
  where alg (LeafF vw) = if vw ^. active
                            then Branch (Split d def) (Leaf vw) (Leaf (vw & active .~ False))
                            else Leaf vw
        alg b = embed b

-- | Split active views horizontally
hSplit :: Window -> Window
hSplit = splitView Hor

-- | Split active views vertically
vSplit :: Window -> Window
vSplit = splitView Vert

-- | Add a new split at the top level in the given direction containing the given buffer.
addSplit :: Dir -> BufRef -> Window -> Window
addSplit d bRef = Branch (def & dir .~ d) (Leaf $ View False bRef)

-- | Close any views which match a given predicate
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

-- | Move focus from any viewports one viewport to the left
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

-- | Move focus from any viewports one viewport to the right
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

-- | Move focus from any viewports one viewport above
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

-- | Move focus from any viewports one viewport below
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

-- | Ensure that there is at least one active viewport
ensureOneActive :: Window -> Window
ensureOneActive w = if not $ anyOf traverse _active w
                       then w & taking 1 traverse . active .~ True
                       else w

-- | Retrieve a tree populated with views and their associated buffer
getBufferViews :: Action (Maybe (BiTree Split (View, Buffer)))
getBufferViews = do
  mWin <- use windows
  case mWin of
    Nothing -> return Nothing
    Just win -> sequence <$> mapM collect win
  where
    collect vw = do
      buf <- getBuffer (vw^.bufRef)
      return $ (,) vw <$> buf
