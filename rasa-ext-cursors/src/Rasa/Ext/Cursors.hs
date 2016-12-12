{-# LANGUAGE DeriveFunctor, TemplateHaskell, OverloadedStrings, Rank2Types #-}

module Rasa.Ext.Cursors
  ( getCursor
  , moveCursorBy
  , moveCursorCoord
  , cursors
  , deleteChar
  , deleteChar'
  , withCursor
  , withCursor'
  , insertText
  , insertText'
  ) where

import Rasa.Ext
import Rasa.Ext.Directive

import Control.Monad
import Control.Lens
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens.Text as TL
import Data.Typeable

-- import Control.Monad.State
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
  c <- getCursor
  bufAttrs foc .= [iattr c (style ReverseVideo), iattr (c+1) (style DefStyle)]


moveCursorBy :: Int -> Alteration ()
moveCursorBy n = do
  foc <- use focused
  mx <- get <&> (^?!bufText foc.to T.length)
  bufExt foc.cursor %= clamp 0 mx . (+n)

moveCursorCoord :: Coord -> Alteration ()
moveCursorCoord coord = do
  foc <- use focused
  txt <- use (bufText foc)
  bufExt foc.cursor.asCoord txt %= addPair coord
  where
    addPair (a, b) (a', b') = (a + a', b + b')

getCursor' :: Int -> Alteration Int
getCursor' bufN = get <&> (^?!bufExt bufN.cursor)

getCursor :: Alteration Int
getCursor = withFocus getCursor'

withCursor :: (Int -> a) -> Alteration a
withCursor f = do
  i <- getCursor
  return $ f i

withCursor' :: Int -> (Int -> a) -> Alteration a
withCursor' bufN f = do
  i <- getCursor' bufN
  return $ f i

withFocus :: (Int -> Alteration a) -> Alteration a
withFocus f = use focused >>= f

deleteChar :: Alteration ()
deleteChar = withFocus deleteChar'

deleteChar' :: Int ->  Alteration ()
deleteChar' bufN = do
  f <- withCursor' bufN deleteCharAt
  bufText bufN %= f

insertText :: T.Text -> Alteration ()
insertText txt = withFocus $ flip insertText' txt

insertText' :: Int -> T.Text -> Alteration ()
insertText' bufN txt = do
  f <- withCursor' bufN $ flip insertTextAt txt
  bufText bufN %= f


-- startOfBuffer :: Int -> Alteration ()
-- startOfBuffer bufN = moveCursorTo bufN 0
--   liftIO $ print $ "New: " ++ show (fromMaybe (-1) new)
--   where trans n' = let f (Cursor i) = Cursor (i + n')
--                     in (pure f <*>)

-- moveCursorTo :: Int -> Int -> Alteration ()
-- moveCursorTo bufN n = do
--   txt <- getBufText bufN
--   let mx = T.length txt
--       curs = clamp 0 mx n
--   setBufExt bufN (Cursor curs)
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
-- startOfLine :: Alteration ()
-- startOfLine = focus $ \curs -> do 
--   foc <- use (editor.focused)
--   let n = focusedBuf.text.before curs.findPrev "\n".to T.length
--   moveCursorBackBy foc n
-- endOfLine :: Alteration ()
-- endOfLine = embed $ focusedBuf %~ findNext' "\n"
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

