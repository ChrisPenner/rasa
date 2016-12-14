module Rasa.Ext.Scheduler
  ( S.Scheduler
  , onInit
  , beforeEvent
  , onEvent
  , beforeRender
  , onRender
  , afterRender
  , onExit
  )
    where

import qualified Rasa.Scheduler as S
import Rasa.Alteration

import Control.Lens
import Control.Monad.Writer
import Data.Default

onInit, beforeEvent, onEvent, beforeRender, onRender, afterRender, onExit :: Alteration () -> S.Scheduler ()

onInit alt = tell (def & S.onInit .~ [alt])
beforeEvent alt = tell (def & S.beforeEvent .~ [alt])
onEvent alt = tell (def & S.onEvent .~ [alt])
beforeRender alt = tell (def & S.beforeRender .~ [alt])
onRender alt = tell (def & S.onRender .~ [alt])
afterRender alt = tell (def & S.afterRender .~ [alt])
onExit alt = tell (def & S.onExit .~ [alt])
