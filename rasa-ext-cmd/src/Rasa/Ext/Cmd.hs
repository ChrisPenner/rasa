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
  Cmd (Map String (String -> App ()))
  deriving (Typeable)

instance Show Cmd where
  show (Cmd cmds) = show (keys cmds)

instance Default Cmd where
  def = Cmd empty

-- It would be nice to make this a little more generic, but I'm not sure how right now.
-- TODO try switching to T.Text -> (T.Text -> a) -> App ()
addCmd :: String -> (String -> App ()) -> App ()
addCmd alias mkEvent =
  stateLens %= add
    where add (Cmd commands) = Cmd $ commands & at alias ?~ mkEvent

runCmd :: String -> String -> App ()
runCmd alias args = do
  Cmd commands <- use stateLens
  let mCmd = commands^.at alias
  case mCmd of
    Just cmd -> cmd args
    Nothing -> return ()
