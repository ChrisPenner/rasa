module Rasa.Internal.Async 
  ( doAsync
  , eventProvider
) where

import Rasa.Internal.Action
import Rasa.Internal.Scheduler

import Data.Typeable
import Control.Concurrent.Async
import Control.Lens
import Control.Monad.IO.Class

-- | This function takes an IO which results in some Event, it runs the IO
-- asynchronously and dispatches the event, then repeats the process all over
-- again. Use this inside the onInit scheduler to register an event listener
-- for some event (e.g. keypresses or network activity)

eventProvider :: Typeable a => IO a -> Action ()
eventProvider getEventIO = doAsync (dispatchAndRerun <$> getEventIO)
    where dispatchAndRerun evt = do
            dispatchEvent evt
            eventProvider getEventIO

-- | doAsync allows you to perform a task asynchronously and then apply the
-- result. In @doAsync asyncAction@, @asyncAction@ is an IO which resolves to
-- an Action, note that the context in which the second action is executed is
-- NOT the same context in which doAsync is called; it is likely that text and
-- other state have changed while the IO executed, so it's a good idea to check
-- (inside the applying Action) that things are in a good state before making
-- changes. Here's an example:
--
-- > asyncCapitalize :: Action ()
-- > asyncCapitalize = do
-- >   txt <- focusDo $ use text
-- >   -- We give doAsync an IO which resolves in an action
-- >   doAsync $ ioPart txt
-- >
-- > ioPart :: Text -> IO (Action ())
-- > ioPart txt = do
-- >   result <- longAsyncronousCapitalizationProgram txt
-- >   -- Note that this returns an Action, but it's still wrapped in IO
-- >   return $ maybeApplyResult txt result
-- >
-- > maybeApplyResult :: Text -> Text -> Action ()
-- > maybeApplyResult oldTxt capitalized = do
-- >   -- We get the current buffer's text, which may have changed since we started
-- >   newTxt <- focusDo (use text)
-- >   if newTxt == oldTxt
-- >     -- If the text is the same as it was, we can apply the transformation
-- >     then focusDo (text .= capitalized)
-- >     -- Otherwise we can choose to re-queue the whole action and try again
-- >     -- Or we could just give up.
-- >     else asyncCapitalize

doAsync ::  IO (Action ()) -> Action ()
doAsync asyncIO = do
  newAsync <- liftIO $ async asyncIO
  asyncs <>= [newAsync]
