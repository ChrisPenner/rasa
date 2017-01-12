{-# Language OverloadedStrings, TemplateHaskell #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Bufs
import Rasa.Ext.Files (save)
import Rasa.Ext.Cursors
import Rasa.Ext.StatusBar

import Control.Monad (unless, void)
import Control.Lens
import Data.Default
import Data.Typeable
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
  maybe (return ()) (`global` keypress) focVimMode

-- | Sets the status bar to the current mode and current VimHist
setStatus :: Action ()
setStatus = void . focusDo $ do
  modeDisp <- use (mode.to show.to Y.fromString)
  histDisp <- use (hist.to show.to Y.fromString)
  centerStatus modeDisp
  rightStatus histDisp

-- | Listeners for keypresses that run regardless of current mode.
global :: VimMode -> Keypress -> Action ()
global Normal (Keypress '+' _) = nextBuf
global Normal (Keypress '-' _) = prevBuf
global _ (Keypress 'c' [Ctrl]) = exit
global _ _ = return ()

-- | Listeners for keypresses when in 'Insert' mode
insert :: [Keypress] -> BufAction ()
insert [Esc] = mode .= Normal
insert [BS] = moveRangesByN (-1) >> delete
insert [Enter] = insertText "\n"
insert [Keypress c _] = insertText (Y.singleton c) >> moveRangesByN 1
insert _ = return ()

-- | Listeners for keypresses when in 'Normal' mode
normal :: [Keypress] -> BufAction ()
normal [Keypress 'i' _] = mode .= Insert
normal [Keypress 'I' _] = startOfLine >> mode .= Insert
normal [Keypress 'a' _] = moveRangesByN 1 >> mode .= Insert
normal [Keypress 'A' _] = endOfLine >> mode .= Insert
normal [Keypress '0' _] = startOfLine
normal [Keypress '$' _] = endOfLine
normal [Keypress 'g' _] = hist <>= [Keypress 'g' []]
normal [Keypress 'g' _,Keypress 'g' _] = ranges .= [Range (Coord 0 0) (Coord 0 1)]

normal [Keypress 'G' _] = do
  txt <- use text
  ranges.= [Range ((Offset $ Y.length txt - 1)^.asCoord txt) ((Offset $ Y.length txt)^.asCoord txt)]

normal [Keypress 'o' _] = endOfLine >> insertText "\n" >> moveRangesByN 1 >> mode .= Insert
normal [Keypress 'O' _] = startOfLine >> insertText "\n" >> mode .= Insert
normal [Keypress 'h' _] = moveRangesByN (-1)
normal [Keypress 'l' _] = moveRangesByN 1
normal [Keypress 'k' _] = moveRangesByC $ Coord (-1) 0
normal [Keypress 'K' _] = rangeDo_ $ addRange . moveRange (Coord (-1) 0)
normal [Keypress 'j' _] = moveRangesByC $ Coord 1 0
normal [Keypress 'J' _] = rangeDo_ $ addRange . moveRange (Coord 1 0)
normal [Keypress 'w' _] = findNext " " >> moveRangesByC (Coord 0 1)
normal [Keypress 'W' _] = rangeDo_ addCursor
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

normal [Keypress 'f' _] = hist <>= [Keypress 'f' []]
normal [Keypress 'f' _,Keypress x []] = findNext $ Y.singleton x
normal [Keypress 't' _] = hist <>= [Keypress 't' []]
normal [Keypress 't' _,Keypress x []] = findNext (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'T' _] = hist <>= [Keypress 'T' []]
normal [Keypress 'T' _,Keypress x []] = findPrev (Y.singleton x)
normal [Keypress 'F' _] = hist <>= [Keypress 'F' []]
normal [Keypress 'F' _,Keypress x []] = findPrev (Y.singleton x) >> moveRangesByN (-1)
normal [Keypress 'X' _] = moveRangesByN (-1) >> delete
normal [Keypress 'x' _] = delete
normal [Keypress 's' [Ctrl]] = save
normal [Keypress ';' _] = ranges <~ use (ranges.reversed.to (take 1))
normal _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = findNext "\n"

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = findPrev "\n"
