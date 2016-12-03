module Rasa.Run (rasa) where

import Rasa.Adapters.Vty (convertEvent, render)
import Rasa.Alteration
import Rasa.Editor

import Control.Lens
import Data.Default (def, Default)
import qualified Graphics.Vty as V

logStore :: Store e -> IO ()
logStore store = appendFile "logfile" ((++ "\n") . show $ store^.event)

handleEvent :: Alteration e () -> Store e -> IO (Store e)
handleEvent = runAlteration

rasa :: Default e => Alteration e () -> IO ()
rasa extensions = do
    writeFile "logfile" "---\n"
    cfg <- V.standardIOConfig
    vty <- V.mkVty cfg
    store <- handleEvent extensions def
    eventLoop extensions vty store

eventLoop :: Alteration e () -> V.Vty -> Store e -> IO ()
eventLoop extensions vty store = do
    logStore store
    sz <- V.displayBounds $ V.outputIface vty
    let pic = V.picForImage $ render sz (store^.editor)
    V.update vty pic
    evt <- convertEvent <$> V.nextEvent vty
    let withEvent = store & event .~ [evt]
    newStore <- handleEvent extensions withEvent
    if newStore^.editor.exiting
       then V.shutdown vty
       else eventLoop extensions vty newStore
