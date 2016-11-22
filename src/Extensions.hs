module Extensions where

import Control.Lens

import Types

applyExtension :: St -> Event -> Extension -> (Extension, [Directive])
applyExtension st evt (Extension name extSt ap) =
    let (newExtState, dirs) = ap extSt st evt
     in (Extension name newExtState ap, dirs)

applyExtensions :: St -> Event -> (St, [Directive])
applyExtensions st evt = (newState, dirs)
    where exts = view extensions st
          newExts = fmap fst applied
          newState = set extensions newExts st
          dirs = concatMap snd applied
          applied = fmap (applyExtension st evt) exts

