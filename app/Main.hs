{-# LANGUAGE OverloadedStrings #-}
module Main where

import VtyAdapter (convertEvent, render)
import Extensions (runExtensions)
import Directives (applyDirectives)
import Types

import Control.Lens
import Data.Default (def)
import qualified Graphics.Vty as V

shouldExit :: [Directive] -> Bool
shouldExit = any isExit

isExit :: Directive -> Bool
isExit Exit = True
isExit _ = False

handleEvent :: Event -> St -> IO ([Extension], [Directive])
handleEvent e st = runAlteration alt (st, e)
    where exts = st^.extensions
          alt :: Alteration [Extension]
          alt = runExtensions exts

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def

eventLoop :: V.Vty -> St -> IO ()
eventLoop vty st = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    (exts, dirs) <- handleEvent evt st
    newState <- applyDirectives st dirs <&> extensions .~ exts
    if shouldExit dirs
       then V.shutdown vty
       else eventLoop vty newState
