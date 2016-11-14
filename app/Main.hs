{-# LANGUAGE OverloadedStrings #-}
module Main where

import Directives
import VtyAdapter (convertEvent, render)
import State

import Control.Lens
import Control.Arrow ((&&&))
import qualified Data.Text.IO as TIO
import Data.Default (def)

import Graphics.Vty as V

appEvent :: St -> V.Event -> Continue
appEvent st evt = handleEvent . toRasa evt $ st

toRasa :: V.Event -> St -> Continue
toRasa e st = Continue (toDirective (st^.mode) (convertEvent e)) st

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def

eventLoop :: V.Vty -> St -> IO ()
eventLoop vty st = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    update vty pic
    e <- V.nextEvent vty
    let (Continue dirs nextState) = appEvent st e
    if Exit `elem` dirs
        then shutdown vty
        else eventLoop vty nextState
