{-# LANGUAGE Rank2Types, TemplateHaskell, OverloadedStrings, ExistentialQuantification, ScopedTypeVariables,
   GeneralizedNewtypeDeriving, FlexibleInstances,
   StandaloneDeriving #-}

module Rasa.Buffer
  ( Buffer
  , Ext(..)
  , bufExts
  , text
  , rope
  , newBuffer
  , useCountFor
  ) where

import Rasa.Text

import qualified Data.Text as T
import qualified Yi.Rope as Y
import Control.Lens hiding (matching)
import Data.Dynamic
import Data.Map

data Ext = forall a. Show a => Ext a

instance Show Ext where
  show (Ext a) = show a

-- | A buffer, holds the text in the buffer and any extension states that are set on the buffer.
-- A buffer is the State of the 'Rasa.Action.BufAction' monad transformer stack, 
-- so the type may be useful in defining lenses over your extension states.
data Buffer = Buffer
  { _rope :: Y.YiString
  , _bufExts :: Map TypeRep Ext
  }

makeLenses ''Buffer

text :: Lens' Buffer T.Text
text = rope.asText

instance Show Buffer where
  show b = "<Buffer {text:" ++ show (b^..text . to (T.take 30)) ++ "...,\n"
           ++ "exts: " ++ show (b^.bufExts) ++ "}>\n"

newBuffer :: T.Text -> Buffer
newBuffer txt =
  Buffer
    { _rope = Y.fromText txt
    , _bufExts = empty
    }

useCountFor :: Lens' Buffer  T.Text
            -> (Int -> Buffer -> Buffer)
            -> Buffer
            -> Buffer
useCountFor l f buf = f curs buf
  where
    curs = buf ^. l . to T.length
