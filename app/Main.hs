{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default (def)

import Events (Event(..), Continue(..), handleEvent)
import VtyAdapter (convertEvent)
import State
import View (render)

import qualified Data.Text.IO as TIO

import Brick.Widgets.Core
import Brick.Widgets.Border

import Control.Lens

import qualified Brick.Types as BT
import qualified Brick.Main as M

appEvent :: St -> BT.BrickEvent () e -> BT.EventM () (BT.Next St)
appEvent st evt = toBrick . handleEvent . toRasa evt $ st

drawUi :: St -> [BT.Widget ()]
drawUi st = [txt (render st)]

toRasa :: BT.BrickEvent () e -> St -> Continue
toRasa (BT.VtyEvent e) st = Continue (convertEvent e) st
toRasa _ st = Continue Noop st

toBrick :: Continue -> BT.EventM () (BT.Next St)
toBrick (Continue e st) = case e of
                                Exit -> M.halt st
                                _ -> M.continue st

app :: M.App St e ()
app =
    M.App { M.appDraw = drawUi
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const def
          }

main :: IO ()
main = TIO.putStr . view text =<< M.defaultMain app def
