{-# LANGUAGE OverloadedStrings #-}
module Main where

import VtyAdapter (convertEvent, render)
import Extensions
import Directives (applyDirectives)
import Control.Monad.State
import Data.Tuple (swap)
import Types as T

import Data.Default (def)

import Graphics.Vty as V

handleEvent :: V.Event -> St -> Continue
handleEvent evt = applyDirectives . toRasa evt

toRasa :: V.Event -> St -> Continue
toRasa e = uncurry Continue . swap . runState (applyExtensions evt)
    where evt = convertEvent e

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
    let (Continue nextState dirs) = handleEvent e st
    if Exit `elem` dirs
       then shutdown vty
       else eventLoop vty nextState
