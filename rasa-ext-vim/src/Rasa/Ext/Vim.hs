{-# Language OverloadedStrings, TemplateHaskell #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Files (save)
import Rasa.Ext.Cursors
import Rasa.Ext.StatusBar
import Rasa.Ext.Views

import Control.Monad (unless)
import Control.Lens
import Data.Text.Lens (packed)
import Data.Default
import Data.Typeable
import qualified Data.Text as T
import qualified Yi.Rope as Y

data VimMode
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimMode where
  def = Normal

newtype VimHist = VimHist
  { _histKeys :: [Keypress]
  } deriving (Show, Typeable)
makeLenses ''VimHist

instance Default VimHist where
  def = VimHist []

-- | A hlens into vim's current mode.
mode :: Lens' Buffer VimMode
mode = bufExt

-- | A lens into the current unresolved keypress history
hist :: Lens' Buffer [Keypress]
hist = bufExt.histKeys

-- | The main export for the vim keybinding extension. Add this to your user config.
--
-- e.g.
--
-- > rasa $ do
-- >    vim
-- >    ...
vim :: Scheduler ()
vim = do
  -- Register to listen for keypresses
  eventListener handleKeypress
  -- Set the status bar to the current mode before each render
  beforeRender setStatus

-- | The event hook which listens for keypresses and responds appropriately
handleKeypress :: Keypress -> Action ()
handleKeypress keypress = do
  focVimMode <- focusDo $ do
    mode' <- use mode
    preHist <- use hist
    case mode' of
      Normal -> normal $ preHist ++ [keypress]
      Insert -> insert $ preHist ++ [keypress]
    postHist <- use hist
    -- | If nothing changed than an action must have happened
    unless (preHist /= postHist) (hist .= [])
    return mode'
  global focVimMode keypress

-- | Sets the status bar to the current mode and current VimHist
setStatus :: Action ()
setStatus = focusDo $ do
  modeDisp <- use (mode.to show.packed)
  histDisp <- use (hist.to show.packed)
  centerStatus modeDisp
  rightStatus histDisp

-- | Listeners for keypresses that run regardless of current mode.
global :: VimMode -> Keypress -> Action ()
global Normal (Keypress '+' _) = nextBuf
global Normal (Keypress '-' _) = prevBuf

global Normal (Keypress 'e' [Ctrl]) = scrollBy 1
global Normal (Keypress 'y' [Ctrl]) = scrollBy (-1)
global Normal (Keypress 'v' [Ctrl]) = vSplit
global Normal (Keypress 'w' [Ctrl]) = hSplit
global Normal (Keypress 'q' [Ctrl]) = closeView

global _ (Keypress 'c' [Ctrl]) = exit
global _ _ = return ()

-- | Listeners for keypresses when in 'Insert' mode
insert :: [Keypress] -> BufAction ()
insert [Esc] = mode .= Normal
insert [BS] = moveRangesByN (-1) >> delete
insert [Enter] = insertText "\n"
insert [Keypress c _] = insertText (T.singleton c) >> moveRangesByN 1
insert _ = return ()

-- | Listeners for keypresses when in 'Normal' mode
normal :: [Keypress] -> BufAction ()
normal [Keypress 'i' []] = mode .= Insert
normal [Keypress 'I' []] = startOfLine >> mode .= Insert
normal [Keypress 'a' []] = moveRangesByN 1 >> mode .= Insert
normal [Keypress 'A' []] = endOfLine >> mode .= Insert
normal [Keypress '0' []] = startOfLine
normal [Keypress '$' []] = endOfLine
normal [Keypress 'g' []] = hist <>= [Keypress 'g' []]
normal [Keypress 'g' [], Keypress 'g' _] = ranges .= [Range (Coord 0 0) (Coord 0 1)]

normal [Keypress 'G' _] = do
  txt <-use rope
  ranges.= [Range ((Offset $ Y.length txt - 1)^.asCoord txt) ((Offset $ Y.length txt)^.asCoord txt)]

normal [Keypress 'o' []] = endOfLine >> insertText "\n" >> moveRangesByN 1 >> mode .= Insert
normal [Keypress 'O' []] = startOfLine >> insertText "\n" >> mode .= Insert
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

normal [Keypress 'b' _] = moveRangesByN (-1) >> findPrev " "
normal [Keypress 'B' _] = rangeDo_ addCursor
  where
    addCursor (Range start _) = do
      next <- findPrevFrom " " start
      let newStart = next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal [Keypress 'f' []] = hist <>= [Keypress 'f' []]
normal [Keypress 'f' [],Keypress x []] = findNext $ T.singleton x
normal [Keypress 't' []] = hist <>= [Keypress 't' []]
normal [Keypress 't' [],Keypress x []] = findNext (T.singleton x) >> moveRangesByN (-1)
normal [Keypress 'T' []] = hist <>= [Keypress 'T' []]
normal [Keypress 'T' [],Keypress x []] = findPrev (T.singleton x)
normal [Keypress 'F' []] = hist <>= [Keypress 'F' []]
normal [Keypress 'F' [],Keypress x []] = findPrev (T.singleton x) >> moveRangesByN (-1)
normal [Keypress 'X' []] = moveRangesByN (-1) >> delete
normal [Keypress 'x' []] = delete
normal [Keypress 's' [Ctrl]] = save
normal [Keypress ';' []] = ranges <~ use (ranges.reversed.to (take 1))
normal _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = findNext "\n"

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = findPrev "\n"
