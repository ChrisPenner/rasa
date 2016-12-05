{-# LANGUAGE TemplateHaskell #-}

module Main where

import Rasa.Run (rasa)
import Rasa.Ext
import Rasa.Ext.Vim.State
import Rasa.Ext.Vim
import Rasa.Adapters.Vty

import Control.Lens
import Data.Default

data ExtState = ExtState
  { _vimSt :: VimSt
  , _vtySt :: VtyState
  }

makeLenses ''ExtState

instance Default ExtState where
  def =
    ExtState
    { _vimSt = def
    , _vtySt = undefined
    }

instance HasVim ExtState where
  vim' = vimSt

instance HasVty ExtState where
  vty' = vtySt

extensions :: Alteration ExtState ()
extensions = do
  vty
  vim

main :: IO ()
main = rasa extensions
