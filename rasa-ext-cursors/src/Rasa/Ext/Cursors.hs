{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Rasa.Ext.Cursors
  ( getCursor
  , moveCursorBy
  , cursors
  , deleteChar
  , deleteChar'
  , withCursor
  , withCursor'
  , insertText
  , insertText'
  ) where

-- import Debug.Trace

import Rasa.Ext
import Rasa.Ext.Directive

import Control.Monad
import Control.Lens
import Control.Monad.State

-- import Control.Lens.Text
import Data.Typeable

-- import Control.Monad.State
import qualified Data.Text as T
-- type Coord = (Int, Int)
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

-- moveCursor :: Int -> Alteration ()
-- moveCursor n = moveCursorBy n
-- moveCursorCoord :: Coord -> Alteration ()
-- moveCursorCoord crd = embed $ focusedBuf %~ moveCursorCoordBy crd
-- startOfBuffer :: Int -> Alteration ()
-- startOfBuffer bufN = moveCursorTo bufN 0
  -- liftIO $ print $ "New: " ++ show (fromMaybe (-1) new)
  -- where trans n' = let f (Cursor i) = Cursor (i + n')
                    -- in (pure f <*>)

-- moveCursorBy' :: Int -> Int -> Alteration ()
-- moveCursorBy' bufN n = do
--   Cursor c <- getBufExt bufN
--   moveCursorTo bufN (c + n)
-- moveCursorCoordBy :: Coord -> Buffer Int -> Buffer Int
-- moveCursorCoordBy c = asCoord . cursor %~ addPair c
--   where
--     addPair (a, b) (a', b') = (a + a', b + b')
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
-- findPrev' :: T.Text -> Buffer Int -> Buffer Int
-- findPrev' txt = useCountFor (withInt before . tillPrev txt) moveCursorBackBy
-- findNext' :: T.Text -> Buffer Int -> Buffer Int
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
-- deleteTillEOL' :: Buffer Int -> Buffer Int
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
