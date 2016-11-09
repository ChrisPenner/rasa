{-# LANGUAGE OverloadedStrings #-}
module Main where

import Directives
import VtyAdapter (convertEvent)
import State
import View (render)

import Control.Lens
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
    let pic = V.picForImage $ text' V.defAttr (render st)
    update vty pic
    e <- V.nextEvent vty
    let (Continue d nextState) = appEvent st e
    case d of
      Exit -> shutdown vty
      _ -> eventLoop vty nextState
