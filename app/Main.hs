{-# LANGUAGE OverloadedStrings #-}
module Main where

import VtyAdapter (convertEvent, render)
import State
import Extensions
import Directives (handleEvent)
import Types as T

import Data.Default (def)

import Graphics.Vty as V

appEvent :: St -> V.Event -> Continue
appEvent st evt = handleEvent $ toRasa evt st

toRasa :: V.Event -> St -> Continue
toRasa e st = uncurry Continue (applyExtensions st (convertEvent e))

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
    let (Continue nextState dirs) = appEvent st e
    if Exit `elem` dirs
       then shutdown vty
       else eventLoop vty nextState
