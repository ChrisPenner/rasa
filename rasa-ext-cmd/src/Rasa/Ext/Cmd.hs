{-# LANGUAGE TemplateHaskell #-}

module Rasa.Ext.Cmd
  ( addCmd
  , runCmd
  ) where

import Rasa.Ext

import Control.Lens
import Data.Map
import Data.Default
import Data.Typeable

data Cmd = Cmd
  { _commands :: Map String (String -> Action ())
  } deriving (Typeable)

instance Show Cmd where
  show (Cmd cmds) = show (keys cmds)

makeLenses ''Cmd

instance Default Cmd where
  def = Cmd
    { _commands=empty
    }

-- It would be nice to make this a little more generic, but I'm not sure how right now.
-- TODO try switching to T.Text -> (T.Text -> a) -> Action ()
addCmd :: String -> (String -> Action ()) -> Action ()
addCmd alias mkEvent = ext.commands.at alias ?= mkEvent

runCmd :: String -> String -> Action ()
runCmd alias args = do
  mCmd <- use (ext.commands.at alias)
  case mCmd of
    Just cmd -> cmd args
    Nothing -> return ()
