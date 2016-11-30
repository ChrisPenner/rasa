module Main where

import VtyAdapter (convertEvent, render)
import Alteration
import Config (runExtensions)
import Event
import State
import ConfigState (ExtState)

import Control.Lens
import Data.Default (def)
import qualified Graphics.Vty as V

handleEvent :: Alteration () -> Maybe Event -> ExtState -> St -> IO Store
handleEvent = runAlteration

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def def

eventLoop :: V.Vty -> St -> ExtState -> IO ()
eventLoop vty st extSt = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    store <- handleEvent runExtensions (Just evt) extSt st
    let newState = store^.editor
        newExtState = store^.extState
    if newState^.exiting
       then V.shutdown vty
       else eventLoop vty newState newExtState
