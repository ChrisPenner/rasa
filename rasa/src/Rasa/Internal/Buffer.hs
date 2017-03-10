{-# language
     Rank2Types
  , TemplateHaskell
  , OverloadedStrings
  , ExistentialQuantification
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  , FlexibleInstances
  , StandaloneDeriving
  #-}

module Rasa.Internal.Buffer
  ( Buffer
  , BufAction
  , HasBuffer(..)
  , BufRef(..)
  , text
  , mkBuffer
  , ref

  , buffers
  , nextBufId

  ) where


import Eve

import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List
import Data.Default

-- | An opaque reference to a buffer.
-- When operating over a BufRef Rasa checks if the 'Rasa.Internal.Buffer.Buffer' still
-- exists and simply ignores any operations over non-existent buffers; typically returning 'Nothing'
newtype BufRef =
  BufRef Int
  deriving (Show, Eq, Ord)

newtype NextBufId = NextBufId
  { _nextBufId' :: Int 
  } deriving Show

instance Default NextBufId where
  def = NextBufId 0

makeLenses ''NextBufId
nextBufId :: HasStates s => Lens' s Int
nextBufId = stateLens.nextBufId'

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
data Buffer = Buffer
  { _text' :: Y.YiString
  , _bufStates' :: States
  , _ref :: BufRef
  }
makeLenses ''Buffer

instance HasStates Buffer where
  states = bufStates'

instance HasEvents Buffer where

-- | This allows creation of polymorphic lenses over any type which has access to a Buffer
class HasBuffer a where
  buffer :: Lens' a Buffer

instance HasBuffer Buffer where
  buffer = lens id (flip const)

-- | This lens focuses the text of the in-scope buffer.
text :: HasBuffer b => Lens' b Y.YiString
text = buffer.text'

instance Show Buffer where
  show b = "text:" ++ (Y.toString . Y.take 30 $ (b^.text)) ++ "...,\n"
           ++ "exts: " ++ extText ++ "}>\n"
    where
      extText = intercalate "\n" $ show <$> b^.states.to M.toList

type BufAction a = Action Buffer a

newtype Buffers = Buffers
  { _buffers' :: IM.IntMap Buffer
  } deriving Show
makeLenses '' Buffers

instance Default Buffers where
  def = Buffers mempty

-- | A lens over the map of available buffers
buffers :: HasStates s => Lens' s (IM.IntMap Buffer)
buffers = stateLens.buffers'

-- | Creates a new buffer from the given text.
mkBuffer :: Y.YiString -> BufRef -> Buffer
mkBuffer txt bRef =
  Buffer
    { _text' = txt
    , _bufStates' = mempty
    , _ref = bRef
    }
