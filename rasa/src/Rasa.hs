{-# language ExistentialQuantification, Rank2Types, ScopedTypeVariables #-}
module Rasa (rasa) where

import Eve
import Rasa.Internal.Listeners

import Control.Monad

-- | The main function to run rasa.
--
-- @rasa eventProviders extensions@
--
-- This should be imported by a user-config with and called with an 'Action'
-- containing any extensions which have event listeners.
--
-- > rasa $ do
-- >   cursor
-- >   vim
-- >   slate

rasa :: App () -> IO ()
rasa initialization = void $ eve (initialization >> hooks)
  where hooks = beforeEvent_ $ do
          dispatchBeforeRender
          dispatchOnRender
          dispatchAfterRender
