{-# LANGUAGE OverloadedStrings #-}
module Main where

import Extensions (extensions)
import VtyAdapter (convertEvent, render)
import State
import Directives (Continue(..), Directive(..), handleEvent)

import Control.Lens
import Control.Monad (when)
import Control.Arrow ((&&&))
import qualified Data.Text.IO as TIO
import Data.Default (def)

import Graphics.Vty as V

appEvent :: St -> V.Event -> Continue
appEvent st evt = handleEvent . toRasa evt $ st

toRasa :: V.Event -> St -> Continue
toRasa e st = Continue (dirsFromEvents (convertEvent e) st) st
    where dirsFromEvents evt st = concatMap (\ext -> snd $ ext st evt) extensions

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
