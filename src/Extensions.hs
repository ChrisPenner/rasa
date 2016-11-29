module Extensions where

import Types

runExtensions :: [Extension] -> Alteration [Extension]
runExtensions = traverse runExtension
