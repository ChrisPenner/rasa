module Main where

import VtyAdapter (convertEvent, render)
import Directives (applyDirectives)
import Config (runExtensions, ExtType)
import Types

import Data.Default (def)
import qualified Graphics.Vty as V

shouldExit :: [Directive] -> Bool
shouldExit = any isExit
    where isExit Exit = True
          isExit _ = False

handleEvent :: Alteration ExtType -> Maybe Event -> St -> IO (ExtType, [Directive])
handleEvent = runAlteration

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def def

eventLoop :: V.Vty -> St -> ExtType -> IO ()
eventLoop vty st extState = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    (newExtState, dirs) <- handleEvent (runExtensions extState) (Just evt) st
    newState <- applyDirectives st dirs
    if shouldExit dirs
       then V.shutdown vty
       else eventLoop vty newState newExtState
