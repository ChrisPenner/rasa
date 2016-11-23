module Extensions where

import Control.Lens
import Control.Monad.State
import Control.Arrow (second)

import Types

applyExtension :: St -> Event -> State Extension [Directive]
applyExtension st evt = state $ \(Extension name appl extSt) ->
    let primed = appl st evt
     in second (Extension name appl) $ runState primed extSt

applyExtensions :: Event -> State St [Directive]
applyExtensions evt = state $ \st ->
    let exts = view extensions st
        newExts = fmap snd applied
        newState = set extensions newExts st
        dirs = concatMap fst applied
        applied = fmap (runState $ applyExtension st evt) exts
     in (dirs, newState)
