{-# Language OverloadedStrings #-}
module Rasa.Ext.Vim
  ( vim
  ) where

import Rasa.Ext
import Rasa.Ext.Files (save)
import Rasa.Ext.Cursors
import Rasa.Ext.StatusBar

import Control.Monad.IO.Class
import Control.Lens
import Data.Text.Lens (packed)
import Data.Default
import Data.Typeable
import qualified Data.Text as T
import qualified Yi.Rope as Y

data VimSt
  = Normal
  | Insert
  deriving (Show, Typeable)

instance Default VimSt where
  def = Normal

-- | A helper to extract the vim state from the current buffer.
-- Specifying the type is what allows it to work.
getVim :: BufAction VimSt
getVim = use bufExt

-- | A helper to set the current VimSt to the new mode
setMode :: VimSt -> BufAction ()
setMode vimst = bufExt .= vimst

-- | The main export for the vim keybinding extension. Add this to your user config.
--
-- e.g.
--
-- > rasa [keypressProvider] $ do
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
  focMode <- focusDo $ do
    mode <- getVim
    case mode of
      Normal -> normal keypress
      Insert -> insert keypress
    return mode
  global focMode keypress

-- | Sets the status bar to the current mode
setStatus :: Action ()
setStatus = focusDo $ do
  mode <- getVim
  centerStatus $ show mode^.packed

-- | Listeners for keypresses that run regardless of current mode.
global :: VimSt -> Keypress -> Action ()
global Normal (Keypress '+' _) = nextBuf
global Normal (Keypress '-' _) = prevBuf
global _ (Keypress 'c' [Ctrl]) = exit
global _ _ = return ()

-- | Listeners for keypresses when in 'Insert' mode
insert :: Keypress -> BufAction ()
insert Esc = setMode Normal
insert BS = moveRangesByN (-1) >> delete
insert Enter = insertText "\n"
insert (Keypress c _) = insertText (T.singleton c) >> moveRangesByN 1
insert _ = return ()

-- | Listeners for keypresses when in 'Normal' mode
normal :: Keypress -> BufAction ()
normal (Keypress 'i' _) = setMode Insert
normal (Keypress 'I' _) = startOfLine >> setMode Insert
normal (Keypress 'a' _) = moveRangesByN 1 >> setMode Insert
normal (Keypress 'A' _) = endOfLine >> setMode Insert
normal (Keypress '0' _) = startOfLine
normal (Keypress '$' _) = endOfLine
normal (Keypress 'g' _) = ranges .= [Range (Coord 0 0) (Coord 0 1)]

normal (Keypress 'G' _) = do
  txt <- use rope
  ranges .= [Range ((Offset $ Y.length txt - 1)^.asCoord txt) ((Offset $ Y.length txt)^.asCoord txt)]

normal (Keypress 'o' _) = endOfLine >> insertText "\n" >> moveRangesByN 1 >> setMode Insert
normal (Keypress 'O' _) = startOfLine >> insertText "\n" >> setMode Insert
normal (Keypress 'h' _) = moveRangesByN (-1)
normal (Keypress 'l' _) = moveRangesByN 1
normal (Keypress 'k' _) = moveRangesByC $ Coord (-1) 0
normal (Keypress 'K' _) = rangeDo_ $ addRange . moveRange (Coord (-1) 0)
normal (Keypress 'j' _) = moveRangesByC $ Coord 1 0
normal (Keypress 'J' _) = rangeDo_ $ addRange . moveRange (Coord 1 0)
normal (Keypress 'w' _) = findNext " " >> moveRangesByC (Coord 0 1)
normal (Keypress 'W' _) = rangeDo_ addCursor
  where
    addCursor (Range _ end) = do
      next <- findNextFrom " " end
      let newStart = moveCursorByN 1 next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal (Keypress 'B' _) = rangeDo_ addCursor
  where
    addCursor (Range start _) = do
      next <- findPrevFrom " " start
      let newStart = next
          newEnd = moveCursorByN 1 newStart
      addRange $ Range newStart newEnd

normal (Keypress 'b' _) = moveRangesByN (-1) >> findPrev " "
normal (Keypress 'f' _) = findNext "f"
normal (Keypress 'F' _) = findPrev "f"
normal (Keypress 'X' _) = moveRangesByN (-1) >> delete
normal (Keypress 'x' _) = delete
normal (Keypress 's' [Ctrl]) = save
normal (Keypress ';' _) = ranges <~ use (ranges.reversed.to (take 1))
normal _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = findNext "\n"

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = findPrev "\n"

