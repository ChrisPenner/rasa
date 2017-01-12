{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   GeneralizedNewtypeDeriving, FlexibleInstances,
   StandaloneDeriving #-}

module Rasa.Internal.Buffer
  ( Buffer
  , Ext(..)
  , bufExts
  -- | A lens over the extensions stored for a buffer
  , text
-- | A lens into the text of the given buffer. Use within a BufAction.
  , mkBuffer
  ) where

import Rasa.Internal.Extensions

import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import Data.Map

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
-- A buffer is the State of the 'Rasa.Internal.Action.BufAction' monad transformer stack,
-- so the type may be useful in defining lenses over your extension states.
data Buffer = Buffer
  { _text :: Y.YiString
  , _bufExts :: ExtMap
  }

makeLenses ''Buffer

instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text . to (Y.take 30)) ++ "...,\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

-- | Creates a new buffer from the givven text.
mkBuffer :: Y.YiString -> Buffer
mkBuffer txt =
  Buffer
    { _text = txt
    , _bufExts = empty
    }
