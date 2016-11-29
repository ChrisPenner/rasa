module Main where

import VtyAdapter (convertEvent, render)
import Directive (applyDirectives, Directive(..))
import Alteration
import Config (runExtensions)
import Event
import State
import ConfigState (ExtState)

import Data.Default (def)
import qualified Graphics.Vty as V

shouldExit :: [Directive] -> Bool
shouldExit = any isExit
    where isExit Exit = True
          isExit _ = False

handleEvent :: Alteration () -> Maybe Event -> ExtState -> St -> IO (ExtState, [Directive])
handleEvent = runAlteration

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def def

eventLoop :: V.Vty -> St -> ExtState -> IO ()
eventLoop vty st extState = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    (newExtState, dirs) <- handleEvent runExtensions (Just evt) extState st
    newState <- applyDirectives st dirs
    if shouldExit dirs
       then V.shutdown vty
       else eventLoop vty newState newExtState
