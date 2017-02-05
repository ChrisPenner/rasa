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

data Cmd =
  Cmd (Map String (String -> Action ()))
  deriving (Typeable)

instance Show Cmd where
  show (Cmd cmds) = show (keys cmds)

instance Default Cmd where
  def = Cmd empty

-- It would be nice to make this a little more generic, but I'm not sure how right now.
-- TODO try switching to T.Text -> (T.Text -> a) -> Action ()
addCmd :: String -> (String -> Action ()) -> Action ()
addCmd alias mkEvent =
  overExt add
    where add (Cmd commands) = Cmd $ commands & at alias ?~ mkEvent

runCmd :: String -> String -> Action ()
runCmd alias args = do
  Cmd commands <- getExt
  let mCmd = commands^.at alias
  case mCmd of
    Just cmd -> cmd args
    Nothing -> return ()
