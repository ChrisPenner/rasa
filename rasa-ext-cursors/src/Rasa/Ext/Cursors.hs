{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}

module Rasa.Ext.Cursors
  ( getCursor
  , moveCursorBy
  , cursors
   -- , startOfBuffer
   -- , deleteChar
   -- , insertText
  ) where

-- import Debug.Trace

import Rasa.Ext

import Control.Applicative
import Control.Monad

-- import Rasa.Ext.Directive
import Control.Lens

-- import Control.Lens.Text
import Data.Typeable

-- import Control.Monad.State
-- import qualified Data.Text as T
-- type Coord = (Int, Int)
newtype Cursor = Cursor
  { _cursor :: Int
  } deriving (Show, Typeable)

makeLenses ''Cursor

cursors :: Alteration ()
cursors = return ()

-- newtype EditorCursors = EditorCursors {
--   _focused :: Int
-- } deriving (Show, Typeable)
-- makeLenses ''EditorCursors
-- focus :: (Int -> (T.Text -> T.Text)) -> Alteration ()
-- focus f = do
--   foc <- use (editor.focused)
--   curs <- getCursor foc
--   editor.focusedBuf.text %= f curs
-- onBuf :: Int -> (Int -> (T.Text -> T.Text)) -> Alteration ()
-- onBuf bufN f = do
--   curs <- getCursor bufN
--   editor.focusedBuf.text %= f curs
-- embed :: (Editor -> Editor) -> Alteration ()
-- embed = zoom editor . modify
getCursor' :: Int -> Alteration Int
getCursor' bufN = do
  curs <- preuse (bufExt bufN)
  case join curs of
    Just (Cursor i) -> return i
    Nothing -> bufExt bufN .= (Just $ Cursor 0) >> return 0

getCursor :: Alteration Int
getCursor = do
  foc <- getFocused
  getCursor' foc

-- getFocused :: Alteration Int
-- getFocused = do
--   foc <- preuse (ext._Just.focused)
--   return $ fromMaybe 0 foc
getFocused :: Alteration Int
getFocused = use focused

-- deleteChar :: Alteration ()
-- deleteChar = focus deleteCharAt
-- deleteChar' :: Int ->  Alteration ()
-- deleteChar' bufN = onBuf bufN deleteCharAt
-- insertText :: T.Text -> Alteration ()
-- insertText txt = focus $ flip insertTextAt txt
-- insertText' :: Int -> T.Text -> Alteration ()
-- insertText' bufN txt = onBuf bufN $ flip insertTextAt txt
-- moveCursor :: Int -> Alteration ()
-- moveCursor n = moveCursorBy n
-- moveCursorCoord :: Coord -> Alteration ()
-- moveCursorCoord crd = embed $ focusedBuf %~ moveCursorCoordBy crd
-- startOfBuffer :: Int -> Alteration ()
-- startOfBuffer bufN = moveCursorTo bufN 0
moveCursorBy :: Int -> Alteration ()
moveCursorBy n = do
  foc <- getFocused
  _ <- getCursor' foc
  bufExt foc %= liftA (cursor %~ (+n))
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
-- clamp :: Int -> Int -> Int -> Int
-- clamp mn mx n
--   | n < mn = mn
--   | n > mx = mx
--   | otherwise = n
-- switchBuf :: Int -> Alteration ()
-- switchBuf n =
--   embed $ execState $
--   do currentBuffer <- use focused
--      numBuffers <- use (buffers . to length)
--      focused .= (n + currentBuffer) `mod` numBuffers
