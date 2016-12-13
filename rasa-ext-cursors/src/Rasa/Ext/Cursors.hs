{-# LANGUAGE DeriveFunctor, TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( getCursor
  , moveCursorBy
  , moveCursorTo
  , moveCursorCoord
  , cursors
  , deleteChar
  , withCursor
  , insertText
  , findNext
  , findPrev
  , withFocus
  ) where

-- import Debug.Trace

import Rasa.Ext
import Rasa.Ext.Directive

import Control.Monad
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens.Text as TL
import Data.Typeable

import qualified Data.Text as T
type Coord = (Int, Int)
newtype Cursor = Cursor
  { _curs :: Int
  } deriving (Show, Typeable)

makeLenses ''Cursor

cursor :: Traversal' (Maybe Cursor) Int
cursor = _Just.curs

cursors :: Alteration ()
cursors = do
  evt <- use event
  -- Initialize all buffers
  when (Init `elem` evt) $ allBufExt .= (Just $ Cursor 0)
  foc <- use focused
  c <- withFocus getCursor
  bufAttrs foc .= [iattr c (style ReverseVideo), iattr (c+1) (style DefStyle)]

moveCursorTo :: Int -> Alteration ()
moveCursorTo n = withFocus (moveCursorTo' n)

moveCursorTo' :: Int -> Int -> Alteration ()
moveCursorTo' n bufN = do
  mx <- get <&> (^?!bufText bufN.to T.length)
  bufExt bufN.cursor .= clamp 0 mx n

moveCursorBy :: Int -> Alteration ()
moveCursorBy n = withFocus $ moveCursorBy' n

moveCursorBy' :: Int -> Int -> Alteration ()
moveCursorBy' n bufN = do
  mx <- get <&> (^?!bufText bufN.to T.length)
  bufExt bufN.cursor %= clamp 0 mx . (+n)

moveCursorCoord :: Coord ->  Alteration ()
moveCursorCoord coord = withFocus (moveCursorCoord' coord)

moveCursorCoord' :: Coord -> Int -> Alteration ()
moveCursorCoord' coord bufN = do
  txt <- use (bufText bufN)
  bufExt bufN.cursor.asCoord txt %= addPair coord
  where
    addPair (a, b) (a', b') = (a + a', b + b')

getCursor :: Int -> Alteration Int
getCursor bufN = get <&> (^?!bufExt bufN.cursor)

withCursor :: (Int -> a) -> Int -> Alteration a
withCursor f bufN = do
  i <- getCursor bufN
  return $ f i

withFocus :: (Int -> Alteration a) -> Alteration a
withFocus f = use focused >>= f

deleteChar :: Alteration ()
deleteChar = withFocus deleteChar'

deleteChar' :: Int ->  Alteration ()
deleteChar' bufN = do
  f <- withCursor deleteCharAt bufN
  bufText bufN %= f

insertText :: T.Text -> Alteration ()
insertText txt = withFocus $ flip insertText' txt

insertText' :: Int -> T.Text -> Alteration ()
insertText' bufN txt = do
  f <- withCursor (`insertTextAt` txt) bufN
  bufText bufN %= f

findNext :: T.Text ->  Alteration ()
findNext txt = withFocus (findNext' txt)

findNext' :: T.Text -> Int -> Alteration ()
findNext' txt bufN = do
  c <- getCursor bufN
  n <- get <&> (^?!bufText bufN.after c.tillNext txt.to T.length)
  moveCursorBy' n bufN

findPrev :: T.Text -> Alteration ()
findPrev txt = withFocus $ findPrev' txt

findPrev' :: T.Text -> Int -> Alteration ()
findPrev' txt bufN = do
  c <- getCursor bufN
  n <- get <&> (^?!bufText bufN.before c.tillPrev txt.to T.length)
  moveCursorBy' (-n) bufN


-- startOfBuffer :: Int -> Alteration ()
-- startOfBuffer bufN = moveCursorTo bufN 0
--   liftIO $ print $ "New: " ++ show (fromMaybe (-1) new)
--   where trans n' = let f (Cursor i) = Cursor (i + n')
--                     in (pure f <*>)

-- moveCursorBackBy :: Int -> Alteration ()
-- moveCursorBackBy n = moveCursorBy (-n)
-- moveCursorBackBy' :: Int -> Int -> Alteration ()
-- moveCursorBackBy' bufN n = moveCursorBy' bufN (-n)
-- appendText :: Int -> T.Text -> Alteration()
-- appendText bufN txt = do
--   n <- getCursor bufN
--   editor.buffers.ix bufN.text %= insertTextAt n txt
-- findPrev' :: T.Text -> Buffer -> Buffer 
-- findPrev' txt = useCountFor (withInt before . tillPrev txt) moveCursorBackBy
-- findNext' :: T.Text -> Buffer -> Buffer 
-- findNext' txt = useCountFor (withInt after . tillNext txt) moveCursorBy
-- endOfBuffer :: Alteration ()
-- endOfBuffer bufN = do
--   mTxt <- getBufText bufN 
--   length' <- mTxt^?.to T.length
--   moveCursorTo bufN length'
-- findNext :: T.Text -> Alteration ()
-- findNext txt = embed $ focusedBuf %~ findNext' txt
-- findPrev :: T.Text -> Alteration ()
-- findPrev txt = embed $ focusedBuf %~ findPrev' txt
-- deleteTillEOL' :: Buffer -> Buffer
-- deleteTillEOL' = withInt after . tillNext "\n" .~ ""
-- deleteTillEOL :: Alteration ()
-- deleteTillEOL = embed $ focusedBuf %~ deleteTillEOL'

-- switchBuf :: Int -> Alteration ()
-- switchBuf n =
--   embed $ execState $
--   do currentBuffer <- use focused
--      numBuffers <- use (buffers . to length)
--      focused .= (n + currentBuffer) `mod` numBuffers

clamp :: Int -> Int -> Int -> Int
clamp mn mx n
  | n < mn = mn
  | n > mx = mx
  | otherwise = n

asCoord :: T.Text -> Iso' Int Coord
asCoord txt = iso (toCoord txt) (toOffset txt)

toOffset :: T.Text -> Coord -> Int
toOffset t (row, col) = clamp 0 (T.length t) $ rowCount + clamp 0 rowLen col
  where
    rowCount = t ^. intillNextN row "\n" . to T.length
    rowLen = T.length $ T.lines t ^. ix row

toCoord :: T.Text -> Int -> Coord
toCoord txt offset = flip runReader txt $ do
  row <- view $ before offset . TL.matching "\n" . to T.length
  col <-
    case row of
      0 -> return offset
      _ -> view $ before offset . tillPrev "\n" . to T.length
  return (row, col)

