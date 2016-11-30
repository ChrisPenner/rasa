module Main where

import VtyAdapter (convertEvent, render)
import Alteration
import Config (extensions)
import Editor

import Control.Lens
import Data.Default (def)
import qualified Graphics.Vty as V

logStore :: Store -> IO ()
logStore store = appendFile "logfile" ((++ "\n") . show $ store^.event)

handleEvent :: Alteration () -> Store -> IO Store
handleEvent = runAlteration

main :: IO ()
main = do
    writeFile "logfile" "---\n"
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    store <- handleEvent extensions def
    eventLoop vty store

eventLoop :: V.Vty -> Store -> IO ()
eventLoop vty store = do
    logStore store
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz (store^.editor)
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    let withEvent = store & event .~ Just evt
    newStore <- handleEvent extensions withEvent
    if newStore^.editor.exiting
       then V.shutdown vty
       else eventLoop vty newStore
