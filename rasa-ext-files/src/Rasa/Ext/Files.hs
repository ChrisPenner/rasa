{-# LANGUAGE TemplateHaskell #-}
module Rasa.Ext.Files
  -- ( files
  -- , saveCurrent
  -- ) 
    where

-- import qualified Data.Text.IO as TIO
-- import Control.Lens
-- import System.Environment

-- import Data.String (fromString)
-- import Data.Typeable

-- import Control.Monad.IO.Class
-- import Control.Monad

-- import Rasa.Ext.Directive
-- import Rasa.Ext

-- data BufFileInfo = BufFileInfo {
-- _filename :: String
--                                }
--                                deriving (Typeable, Show, Eq)
-- makeLenses ''BufFileInfo

-- files :: Alteration ()
-- files = do
--   evt <- use event
--   when (Init `elem` evt) loadFiles

-- saveCurrent :: Alteration ()
-- saveCurrent = do
--   -- TODO: Use current buffer from cursor ext
--   -- -- TTODO: Use current buffer from cursor ext
--   txt <- preuse $ bufText 0
--   -- fname <- preuse (filename . bufExt 0)
--   return ()
--   -- let fname = buf ^. filename
--       -- contents = buf ^. text
--   -- liftIO $ TIO.writeFile fname contents

-- loadFiles :: Alteration ()
-- loadFiles = do
--   fileNames <- liftIO getArgs
--   pairs <- zip [0..] fileNames
--   fileTexts <- liftIO $ traverse (TIO.readFile . fromString) fileNames
--   mapM_ addBuffer $
