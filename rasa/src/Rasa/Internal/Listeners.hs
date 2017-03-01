{-# language
  GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , RankNTypes
#-}

module Rasa.Internal.Listeners
  ( beforeEveryRender
  , beforeEveryRender_
  , dispatchBeforeRender

  , onEveryRender
  , onEveryRender_
  , dispatchOnRender

  , afterEveryRender
  , afterEveryRender_
  , dispatchAfterRender

  , onExit

  , onKeypress
  , dispatchKeypress
  ) where

import Reflex
import Rasa.Internal.Buffer
import Rasa.Internal.Events

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Default
import Data.Typeable
import Data.Maybe
import qualified Data.Map as M

-- | Trigger an 'App' on a 'Keypress'
onKeypress :: (Keypress -> App result) -> App ListenerId
onKeypress actionF = addListener (void <$> actionF)

-- | Dispatch a 'Keypress' event.
dispatchKeypress :: Keypress -> App ()
dispatchKeypress = dispatchEvent

-- | Registers an action to be performed BEFORE each render phase.
--
-- This is a good spot to add information useful to the renderer
-- since all actions have been performed. Only cosmetic changes should
-- occur during this phase.
beforeEveryRender :: App a -> App ListenerId
beforeEveryRender action = addListener (const (void action) :: BeforeRender -> App ())

beforeEveryRender_ :: App a -> App ()
beforeEveryRender_ = void . beforeEveryRender

-- | Dispatch the 'BeforeRender' action.
dispatchBeforeRender :: App ()
dispatchBeforeRender = dispatchEvent BeforeRender

-- | Registers an action to be performed during each render phase.
--
-- This phase should only be used by extensions which actually render something.
onEveryRender :: App a -> App ListenerId
onEveryRender action = addListener (const $ void action :: OnRender -> App ())

onEveryRender_ :: App a -> App ()
onEveryRender_ = void . onEveryRender

-- | Dispatch the 'OnRender' action.
dispatchOnRender :: App ()
dispatchOnRender = dispatchEvent OnRender

-- | Registers an action to be performed AFTER each render phase.
--
-- This is useful for cleaning up extension state that was registered for the
-- renderer, but needs to be cleared before the next iteration.
afterEveryRender :: App a -> App ListenerId
afterEveryRender action = addListener (const $ void action :: AfterRender -> App ())

afterEveryRender_ :: App a -> App ()
afterEveryRender_ = void . afterEveryRender

-- | Dispatch the 'AfterRender' action.
dispatchAfterRender :: App ()
dispatchAfterRender = dispatchEvent AfterRender
