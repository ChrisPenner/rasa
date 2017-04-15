{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Views
import Rasa.Ext.Files (save)
import Rasa.Ext.Cursors

import Control.Monad
import Control.Lens
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Typeable
import qualified Yi.Rope as Y

-- | A type representing the current mode of a buffer
data VimMode
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimMode where
  def = Normal

mode :: Lens' View VimMode
mode = stateLens

-- | A history of any keypresses which haven't matched a pattern
newtype VimHist = VimHist 
  { _vimHist' :: [Keypress]
  } deriving (Show, Typeable)
makeLenses ''VimHist

instance Default VimHist where
  def = VimHist []

vimHist :: Lens' View [Keypress]
vimHist = makeStateLens vimHist'

addHist :: Keypress -> ViewAction ()
addHist keypress = vimHist <>= [keypress]

-- | The main export for the vim keybinding extension. Add this to your user config.
--
-- e.g.
--
-- > rasa $ do
-- >    vim
-- >    ...
vim :: App ()
vim = do
  void $ onKeypress handleKeypress
  onNewView . void . addBottomStatus $ do
    mode' <- use mode
    return $ case mode' of
      Normal -> styleText "NORMAL" $ fg Magenta
      Insert -> styleText "INSERT" $ fg Green

-- | The event listener which listens for keypresses and responds appropriately
handleKeypress :: Keypress -> App ()
handleKeypress keypress = activeViewsDo_ $ do
  mode' <- use mode
  preHist <- use vimHist
  case mode' of
    Normal -> normal $ preHist ++ [keypress]
    Insert -> insert $ preHist ++ [keypress]
  anyMode $ preHist ++ [keypress]
  postHist <- use vimHist
  -- If nothing changed than an action must have happened
  unless (preHist /= postHist) (vimHist .= [])

-- | Listeners for keypresses that run regardless of current mode.
anyMode :: [Keypress] -> ViewAction ()
anyMode [Keypress 'c' [Ctrl]] = runApp exit
anyMode [KPageDown []] = runApp . activeViewsDo_ $ scrollBy 14 -- Page down
anyMode [KPageUp []] = runApp . activeViewsDo_ $ scrollBy (-14) -- Page up
anyMode [KHome []] = viewBufDo startOfLine
anyMode [KEnd []] = viewBufDo endOfLine
anyMode [Keypress 'a' [Ctrl]] = runApp $ addRenderableSplit (VRenderable ("hi" :: Y.YiString))
anyMode _ = return ()

-- | Listeners for keypresses when in 'Insert' mode
insert :: [Keypress] -> ViewAction ()
insert [KEsc []] = mode .= Normal

insert [KBS []] = viewBufDo $ moveRangesByN (-1) >> delete
insert [KDel []] = viewBufDo delete
insert [KEnter []] = viewBufDo $ insertText "\n" >> moveRangesByC (Coord 1 0) >> startOfLine
insert [Keypress c []] = viewBufDo $ insertText (Y.singleton c) >> moveRangesByN 1
insert _ = return ()

-- | Listeners for keypresses when in 'Normal' mode
normal :: [Keypress] -> ViewAction ()
normal [Keypress 'i' []] = mode .= Insert
normal [Keypress 'I' []] = viewBufDo startOfLine >> mode .= Insert
normal [Keypress 'a' []] = viewBufDo (moveRangesByN 1) >> mode .= Insert
normal [Keypress 'A' []] = viewBufDo endOfLine >> mode .= Insert
normal [Keypress '0' []] = viewBufDo startOfLine
normal [Keypress '$' []] = viewBufDo endOfLine
normal [Keypress 'g' []] = addHist $ Keypress 'g' []
normal [Keypress 'g' [], Keypress 'g' []] = viewBufDo $ setRanges [Range (Coord 0 0) (Coord 0 1)]
normal [Keypress 's' []] = addHist $ Keypress 's' []
normal [Keypress 's' [], Keypress 'n' []] = viewBufDo toggleLineNumbers

normal [Keypress '+' []] = runApp nextBuf
normal [Keypress '-' []] = runApp prevBuf
normal [Keypress 'w' [Ctrl]] = runApp hSplit
normal [Keypress 'v' [Ctrl]] = runApp vSplit
normal [Keypress 'o' [Ctrl]] = runApp closeInactive
normal [Keypress 'r' [Ctrl]] = runApp rotate

normal [Keypress 'e' [Ctrl]] = runApp . activeViewsDo_ $ scrollBy 1 -- Scroll down
normal [Keypress 'd' [Ctrl]] = runApp . activeViewsDo_ $ scrollBy 7 -- Half-Page down
normal [Keypress 'y' [Ctrl]] = runApp . activeViewsDo_ $ scrollBy (-1) -- Scroll up
normal [Keypress 'u' [Ctrl]] = runApp . activeViewsDo_ $ scrollBy (-7) -- Half-Page up

normal [KLeft []] = runApp focusViewLeft
normal [KRight []] = runApp focusViewRight
normal [KUp []] = runApp focusViewAbove
normal [KDown []] = runApp focusViewBelow


normal [Keypress 'G' []] = viewBufDo $ do
  txt <- getText
  setRanges [Range ((Offset $ Y.length txt - 1)^.asCoord txt) ((Offset $ Y.length txt)^.asCoord txt)]

normal [Keypress 'o' []] = viewBufDo (endOfLine >> insertText "\n" >> moveRangesByC (Coord 1 0)) >> mode .= Insert
normal [Keypress 'O' []] = viewBufDo (startOfLine >> insertText "\n") >> mode .= Insert
normal [Keypress 'h' []] = viewBufDo $ moveRangesByN (-1)
normal [Keypress 'l' []] = viewBufDo $ moveRangesByN 1
normal [Keypress 'k' []] = viewBufDo $ moveRangesByC $ Coord (-1) 0
normal [Keypress 'K' []] = viewBufDo $ rangeDo_ $ addRange . moveRange (Coord (-1) 0)
normal [Keypress 'j' []] = viewBufDo $ moveRangesByC $ Coord 1 0
normal [Keypress 'J' []] = viewBufDo $ rangeDo_ $ addRange . moveRange (Coord 1 0)
normal [Keypress 'w' []] = viewBufDo $ findNext " " >> moveRangesByC (Coord 0 1)
normal [Keypress 'W' []] = viewBufDo $ rangeDo_ addCursor
  where
    addCursor (Range _ end) = do
      next <- findNextFrom " " end
      let newStart = moveCursorByN 1 next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal [Keypress 'b' []] = viewBufDo $ moveRangesByN (-1) >> findPrev " "
normal [Keypress 'B' []] = viewBufDo $ rangeDo_ addCursor
  where
    addCursor (Range start _) = do
      next <- findPrevFrom " " start
      let newStart = next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal [Keypress 'f' []] = addHist $ Keypress 'f' []
normal [Keypress 'f' [],Keypress x []] = viewBufDo $ findNext $ Y.singleton x
normal [Keypress 't' []] = addHist $ Keypress 't' []
normal [Keypress 't' [],Keypress x []] = viewBufDo $ findNext (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'T' []] = addHist $ Keypress 'T' []
normal [Keypress 'T' [],Keypress x []] = viewBufDo $ findPrev (Y.singleton x)
normal [Keypress 'F' []] = addHist $ Keypress 'F' []
normal [Keypress 'F' [],Keypress x []] = viewBufDo $ findPrev (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'X' []] = viewBufDo $ moveRangesByN (-1) >> delete
normal [Keypress 'x' []] = viewBufDo delete
normal [Keypress 's' [Ctrl]] = viewBufDo save
normal [Keypress ';' []] = viewBufDo $ do
  rngs <- getRanges
  setRanges (rngs^.reversed.to (take 1))
normal [Keypress 'r' []] = addHist $ Keypress 'r' []
normal [Keypress 'r' [],Keypress c []] = viewBufDo $ delete >> insertText (Y.singleton c)
normal _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = do
  txt <- getText
  overRanges . map $ overBoth $ coordEndOfLine txt
  where
    coordEndOfLine :: Y.YiString -> Coord -> Coord
    coordEndOfLine txt (Coord row col) = Coord row maxColumn
      where
        maxColumn :: Int
        maxColumn = fromMaybe col (txt ^? asLines . ix row . to Y.length)

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = overRanges . map $ overBoth coordStartOfLine
  where
    coordStartOfLine :: Coord -> Coord
    coordStartOfLine (Coord x _) = Coord x 0
