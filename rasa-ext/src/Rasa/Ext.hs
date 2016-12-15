{-# LANGUAGE Rank2Types, FlexibleContexts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Rasa.Ext
-- Copyright   :  (C) 2016 Chris Penner
-- License     :  MIT
-- Maintainer  :  Chris Penner <christopher.penner@gmail.com>
--
-- This module and its descendents contain the public API for building an extension
-- for Rasa. It mostly just re-exports the parts of rasa-core that are considered 
-- public API.
-- 
-- Also see 'Rasa.Ext.Scheduler', 'Rasa.Ext.Directive'
----------------------------------------------------------------------------
module Rasa.Ext
  ( 
  -- * Performing Actions
  Action
  , BufAction

  -- * Storing Extension State
  , ext
  , bufExt

  -- * Accessing/Editing Context
  , text
  , event
  , exiting

  -- * Useful Types
  , Event(..)
  , Mod(..)
  , Buffer
  ) where

import Rasa.Action
import Rasa.State
import Rasa.Events
import Rasa.Buffer
