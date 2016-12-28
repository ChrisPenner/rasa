{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   GeneralizedNewtypeDeriving, FlexibleInstances,
   StandaloneDeriving #-}

module Rasa.Internal.Buffer
  ( Buffer
  , Ext(..)
  , bufExts
  -- | A lens over the extensions stored for a buffer
  , text
  , rope
  -- | A lens over the text stored in a buffer (as a 'Y.YiString')
  , newBuffer
  ) where

import Rasa.Internal.Text
import Rasa.Internal.Extensions

import qualified Data.Text as T
import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import Data.Map

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
-- A buffer is the State of the 'Rasa.Internal.Action.BufAction' monad transformer stack,
-- so the type may be useful in defining lenses over your extension states.
data Buffer = Buffer
  { _rope :: Y.YiString
  , _bufExts :: ExtMap
  }

makeLenses ''Buffer

-- | A lens into the text of the given buffer. Use within a BufAction.
text :: Lens' Buffer T.Text
text = rope.asText

instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text . to (T.take 30)) ++ "...,\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

-- | Creates a new buffer from the givven text.
newBuffer :: T.Text -> Buffer
newBuffer txt =
  Buffer
    { _rope = Y.fromText txt
    , _bufExts = empty
    }
