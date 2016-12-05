{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Rasa.Adapters.Vty (vty, VtyState, HasVty, vty')
    where

import Rasa.Editor
import Rasa.Buffer
import Rasa.View
import Rasa.Event
import Rasa.Ext

import qualified Graphics.Vty as V
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad
import Data.Monoid ((<>))
import Control.Arrow (first)

type VtyState = V.Vty

class HasVty e where
    vty' :: Lens' e V.Vty

vty :: HasVty e => Alteration e ()
vty = do
    evt <- getEvent
    when (Init `elem` evt) initUi
    if Exit `elem` evt then shutdown
                       else render >> nextEvent

initUi :: HasVty e => Alteration e ()
initUi = do
    cfg <- liftIO V.standardIOConfig
    v <- liftIO $ V.mkVty cfg
    setPlugin vty' v

getSize :: MonadIO m => V.Vty -> m (Int, Int)
getSize v = liftIO $ V.displayBounds $ V.outputIface v

nextEvent :: HasVty e => Alteration e ()
nextEvent = do
    v <- getPlugin vty'
    evt <- liftIO $ convertEvent <$> V.nextEvent v
    setEvent [evt]

shutdown :: HasVty e => Alteration e ()
shutdown = do
    v <- getPlugin vty'
    liftIO $ V.shutdown v

render :: HasVty e => Alteration e ()
render = do
    v <- getPlugin vty'
    editor <- getState
    sz <- getSize v
    let pic = V.picForImage $ render' sz editor
    liftIO $ V.update v pic

class Renderable a b where
    render' :: Size -> a -> b

convertEvent :: V.Event -> Event
convertEvent (V.EvKey e mods) = convertKeypress e mods
convertEvent _ = Unknown

convertKeypress :: V.Key -> [V.Modifier] -> Event
convertKeypress V.KEnter _ = Enter
convertKeypress V.KBS _ = BS
convertKeypress V.KEsc _ = Esc
convertKeypress (V.KChar c) mods  = Keypress c (fmap convertMod mods)
convertKeypress _ _  = Unknown

convertMod :: V.Modifier -> Mod
convertMod m = case m of
                 V.MShift -> Shift
                 V.MCtrl -> Ctrl
                 V.MMeta -> Alt
                 V.MAlt -> Alt

instance Renderable Editor V.Image where
    render' sz = view $ focusedBuf . to (render' sz)

instance Renderable (Buffer Offset) V.Image where
    render' (width, _) = do
        txt <- textWrap width . view text
        curs <- view cursor
        coord <- view $ asCoord.cursor
        let styled = applyAttrs [(curs, inverse), (curs + 1, V.defAttr)] txt
            txtLines = foldMap (V.<->) styled V.emptyImage
            cursorLine = V.text' V.defAttr (T.pack $ show curs <> " | " <> show coord)

        return $ cursorLine V.<-> txtLines

            where inverse = V.currentAttr `V.withStyle` V.reverseVideo

applyAttrs :: [(Offset, V.Attr)] -> T.Text -> [V.Image]
applyAttrs attrs t = applyAttrs' attrs (T.lines t)

applyAttrs' :: [(Offset, V.Attr)] -> [T.Text] -> [V.Image]
applyAttrs' _ [] = []
applyAttrs' [] lines' = fmap (V.text' V.currentAttr) lines'
applyAttrs' allAttrs@((offset, attr):attrs) (l:lines')
  | T.length l < offset = plainText l : applyAttrs' (decr (T.length l + 1) allAttrs) lines'
  | otherwise = let prefix = plainText (T.take offset l) V.<|> V.text' attr ""
                    suffix = if null rest
                                then V.emptyImage
                                else head rest
                    rest = applyAttrs' (decr offset attrs) (T.drop offset l:lines')
                 in (prefix V.<|> suffix) : drop 1 rest

decr :: Int ->  [(Offset, V.Attr)] -> [(Offset, V.Attr)]
decr n = fmap $ first (subtract n)

plainText :: T.Text -> V.Image
plainText = V.text' V.currentAttr
