{-# LANGUAGE OverloadedStrings #-}
module Main where

import VtyAdapter (convertEvent, render)
import Extensions
import Directives (applyDirectives)
import Control.Monad.State
import Types

import Data.Default (def)
import Graphics.Vty as V

shouldExit :: [Directive] -> Bool
shouldExit = any isExit

isExit :: Directive -> Bool
isExit Exit = True
isExit _ = False

handleEvent :: V.Event -> St -> (Bool, IO St)
handleEvent e st = (shouldExit dirs, applyDirectives newState dirs)
    where evt = convertEvent e
          (dirs, newState) = runState (applyExtensions evt) st

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
    let (exit, getNewState) = handleEvent e st
    newState <- getNewState
    if exit
       then shutdown vty
       else eventLoop vty newState
