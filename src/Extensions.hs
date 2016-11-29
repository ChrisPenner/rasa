module Extensions where

import Types

runExtension :: Extension -> Alteration Extension
runExtension Extension{_extState=extSt, _apply=appl} = appl extSt

runExtensions :: [Extension] -> Alteration [Extension]
runExtensions = traverse runExtension
