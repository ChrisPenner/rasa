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
  , HasBuffer(..)
  , text
  , mkBuffer
  ) where

import Rasa.Internal.Extensions

import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import Data.Default
import Data.Map as M
import Data.List

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
data Buffer = Buffer
  { _text' :: Y.YiString
  , _bufExts' :: ExtMap
  }
makeLenses ''Buffer

instance Default Buffer where
  def = Buffer def def

instance HasExts Buffer where
  exts = bufExts'

instance Show Buffer where
  show b = "text:" ++ (Y.toString . Y.take 30 $ (b^.text)) ++ "...,\n"
           ++ "exts: " ++ extText ++ "}>\n"
    where
      extText = intercalate "\n" $ show <$> b^.exts.to M.toList

-- | This allows creation of polymorphic lenses over any type which has access to a Buffer
class HasBuffer a where
  buffer :: Lens' a Buffer

instance HasBuffer Buffer where
  buffer = lens id (flip const)

-- | This lens focuses the text of the in-scope buffer.
text :: HasBuffer b => Lens' b Y.YiString
text = buffer.text'

-- | Creates a new buffer from the given text.
mkBuffer :: Y.YiString -> Buffer
mkBuffer txt =
  Buffer
    { _text' = txt
    , _bufExts' = empty
    }
