module Rasa.Internal.Async (
  -- doAsync
  eventProvider
) where

import Rasa.Internal.Action
import Rasa.Internal.Scheduler

import Data.Typeable
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class

-- | This function takes an IO which results in some Event, it runs the IO asynchronously and dispatches the event,
-- then repeats the process all over again.
-- Use this inside the onInit scheduler to register an event listener for some event (e.g. keypresses or network
-- activity)
eventProvider :: (Typeable a, Show a) => IO a -> Action ()
eventProvider getEventIO = do
  theAsync <- liftIO $ async asyncActionIO
  asyncs <>= [theAsync]
    where asyncActionIO = do
            evt <- getEventIO
            -- When this action runs, queue the same event listener recursively
            return $ do
              dispatchEvent evt
              eventProvider getEventIO
