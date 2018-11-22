module Rasa.Ext.Files.Internal.Events where

import Rasa.Ext

import Control.Monad

data Extension = Extension String | UnknownExtension deriving (Eq)

data FileLoaded
  = FileLoaded Extension BufRef

-- | Trigger an 'App' on a 'Keypress'
onFileLoaded :: (FileLoaded -> App result) -> App ListenerId
onFileLoaded actionF = addListener (void <$> actionF)

-- | Dispatch a 'Keypress' event.
dispatchFileLoaded :: FileLoaded -> App ()
dispatchFileLoaded = dispatchEvent
