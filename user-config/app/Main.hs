module Main where

import Rasa.Run (rasa)
import Config (extensions)

main :: IO ()
main = rasa extensions

