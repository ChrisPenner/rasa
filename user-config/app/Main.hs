module Main where

import Rasa.Run (rasa)
import Config (extensions)
import Rasa.Adapters.Vty (renderer)

main :: IO ()
main = rasa renderer extensions
