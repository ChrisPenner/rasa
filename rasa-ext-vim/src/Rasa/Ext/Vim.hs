{-# Language OverloadedStrings #-}
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
import Data.Typeable
import qualified Yi.Rope as Y

-- | A type representing the current mode of a buffer
data VimMode
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimMode where
  def = Normal

getMode :: BufAction VimMode
getMode = getBufExt

setMode :: VimMode -> BufAction ()
setMode = setBufExt

-- | A history of any keypresses which haven't matched a pattern
newtype VimHist =
  VimHist [Keypress]
  deriving (Show, Typeable)

instance Default VimHist where
  def = VimHist []

addHist :: Keypress -> BufAction ()
addHist keypress = overBufExt extend
  where extend (VimHist hist) = VimHist $ hist ++ [keypress]

getHist :: BufAction [Keypress]
getHist = do
  VimHist h <- getBufExt
  return h

setHist :: [Keypress] -> BufAction ()
setHist = setBufExt . VimHist

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
  onEveryNewBuffer_ . addBottomStatus $ do
    mode <- getMode
    return $ case mode of
      Normal -> styleText "NORMAL" $ fg Magenta
      Insert -> styleText "INSERT" $ fg Green

-- | The event listener which listens for keypresses and responds appropriately
handleKeypress :: Keypress -> App ()
handleKeypress keypress = focusDo_ $ do
  mode <- getMode
  preHist <- getHist
  case mode of
    Normal -> normal $ preHist ++ [keypress]
    Insert -> insert $ preHist ++ [keypress]
  anyMode $ preHist ++ [keypress]
  postHist <- getHist
  -- If nothing changed than an action must have happened
  unless (preHist /= postHist) (setHist [])

-- | Listeners for keypresses that run regardless of current mode.
anyMode :: [Keypress] -> BufAction ()
anyMode [Keypress 'c' [Ctrl]] = liftApp exit
anyMode [KPageDown []] = liftApp $ scrollBy 14 -- Page down
anyMode [KPageUp []] = liftApp $ scrollBy (-14) -- Page up
anyMode [KHome []] = startOfLine
anyMode [KEnd []] = endOfLine
anyMode _ = return ()

-- | Listeners for keypresses when in 'Insert' mode
insert :: [Keypress] -> BufAction ()
insert [KEsc []] = setMode Normal

insert [KBS []] = moveRangesByN (-1) >> delete
insert [KDel []] = delete
insert [KEnter []] = insertText "\n" >> moveRangesByC (Coord 1 0) >> startOfLine
insert [Keypress c []] = insertText (Y.singleton c) >> moveRangesByN 1
insert _ = return ()

-- | Listeners for keypresses when in 'Normal' mode
normal :: [Keypress] -> BufAction ()
normal [Keypress 'i' []] = setMode Insert
normal [Keypress 'I' []] = startOfLine >> setMode Insert
normal [Keypress 'a' []] = moveRangesByN 1 >> setMode Insert
normal [Keypress 'A' []] = endOfLine >> setMode Insert
normal [Keypress '0' []] = startOfLine
normal [Keypress '$' []] = endOfLine
normal [Keypress 'g' []] = addHist $ Keypress 'g' []
normal [Keypress 'g' [], Keypress 'g' []] = setRanges [Range (Coord 0 0) (Coord 0 1)]
normal [Keypress 's' []] = addHist $ Keypress 's' []
normal [Keypress 's' [], Keypress 'n' []] = toggleLineNumbers

normal [Keypress '+' []] = liftApp nextBuf
normal [Keypress '-' []] = liftApp prevBuf
normal [Keypress 'w' [Ctrl]] = liftApp hSplit
normal [Keypress 'v' [Ctrl]] = liftApp vSplit
normal [Keypress 'o' [Ctrl]] = liftApp closeInactive
normal [Keypress 'r' [Ctrl]] = liftApp rotate

normal [Keypress 'e' [Ctrl]] = liftApp $ scrollBy 1 -- Scroll down
normal [Keypress 'd' [Ctrl]] = liftApp $ scrollBy 7 -- Half-Page down
normal [Keypress 'y' [Ctrl]] = liftApp $ scrollBy (-1) -- Scroll up
normal [Keypress 'u' [Ctrl]] = liftApp $ scrollBy (-7) -- Half-Page up

normal [KLeft []] = liftApp focusViewLeft
normal [KRight []] = liftApp focusViewRight
normal [KUp []] = liftApp focusViewAbove
normal [KDown []] = liftApp focusViewBelow


normal [Keypress 'G' []] = do
  txt <- getText
  setRanges [Range ((Offset $ Y.length txt - 1)^.asCoord txt) ((Offset $ Y.length txt)^.asCoord txt)]

normal [Keypress 'o' []] = endOfLine >> insertText "\n" >> moveRangesByC (Coord 1 0) >> setMode Insert
normal [Keypress 'O' []] = startOfLine >> insertText "\n" >> setMode Insert
normal [Keypress 'h' []] = moveRangesByN (-1)
normal [Keypress 'l' []] = moveRangesByN 1
normal [Keypress 'k' []] = moveRangesByC $ Coord (-1) 0
normal [Keypress 'K' []] = rangeDo_ $ addRange . moveRange (Coord (-1) 0)
normal [Keypress 'j' []] = moveRangesByC $ Coord 1 0
normal [Keypress 'J' []] = rangeDo_ $ addRange . moveRange (Coord 1 0)
normal [Keypress 'w' []] = findNext " " >> moveRangesByC (Coord 0 1)
normal [Keypress 'W' []] = rangeDo_ addCursor
  where
    addCursor (Range _ end) = do
      next <- findNextFrom " " end
      let newStart = moveCursorByN 1 next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal [Keypress 'b' []] = moveRangesByN (-1) >> findPrev " "
normal [Keypress 'B' []] = rangeDo_ addCursor
  where
    addCursor (Range start _) = do
      next <- findPrevFrom " " start
      let newStart = next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal [Keypress 'f' []] = addHist $ Keypress 'f' []
normal [Keypress 'f' [],Keypress x []] = findNext $ Y.singleton x
normal [Keypress 't' []] = addHist $ Keypress 't' []
normal [Keypress 't' [],Keypress x []] = findNext (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'T' []] = addHist $ Keypress 'T' []
normal [Keypress 'T' [],Keypress x []] = findPrev (Y.singleton x)
normal [Keypress 'F' []] = addHist $ Keypress 'F' []
normal [Keypress 'F' [],Keypress x []] = findPrev (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'X' []] = moveRangesByN (-1) >> delete
normal [Keypress 'x' []] = delete
normal [Keypress 's' [Ctrl]] = save
normal [Keypress ';' []] = do
  rngs <- getRanges
  setRanges (rngs^.reversed.to (take 1))
normal [Keypress 'r' []] = addHist $ Keypress 'r' []
normal [Keypress 'r' [],Keypress c []] = delete >> insertText (Y.singleton c)
normal _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = findNext "\n"

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = findPrev "\n"
