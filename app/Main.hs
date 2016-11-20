{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config (extensions)
import VtyAdapter (convertEvent, render)
import State
import Directives (handleEvent)
import Types as T

import Control.Lens
import Control.Monad (when)
import Control.Arrow ((&&&))
import qualified Data.Text.IO as TIO
import Data.Default (def)

import Graphics.Vty as V

appEvent :: St -> [Extension] -> V.Event -> Continue
appEvent st exts evt = handleEvent $ toRasa evt st exts

toRasa :: V.Event -> St -> [Extension] -> Continue
toRasa e st exts = uncurry Continue (applyExtensions st exts (convertEvent e)) st

applyExtensions :: St -> [Extension] -> T.Event -> ([Extension], [Directive])
applyExtensions st exts evt = (newExts, dirs)
    where newExts = fmap fst applied
          dirs = concatMap snd applied
          applied = fmap (applyExtension st evt) exts

main :: IO ()
main = do
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    eventLoop vty def extensions

eventLoop :: V.Vty -> St -> [Extension] -> IO ()
eventLoop vty st exts = do
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz st
    update vty pic
    e <- V.nextEvent vty
    let (Continue newExts dirs nextState) = appEvent st exts e
    if Exit `elem` dirs
       then shutdown vty
       else eventLoop vty nextState newExts
