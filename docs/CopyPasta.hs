-- This is the resulting source code from the
-- 'Building an Extension' tutorial
module Main where

import Rasa (rasa)
import Rasa.Ext
import Rasa.Ext.Cursors
import Rasa.Ext.Files
import Rasa.Ext.Logger
import Rasa.Ext.Slate
import Rasa.Ext.StatusBar
import Rasa.Ext.Style
import Rasa.Ext.Views
import Rasa.Ext.Vim

import Control.Monad.IO.Class
import Control.Lens
import Data.Default
import qualified Data.Text as T

newtype CopyPasta = CopyPasta String
  deriving Show

instance Default CopyPasta where
  def = CopyPasta ""

newtype Copied = Copied String

copyPasta :: Keypress -> Action ()
copyPasta (Keypress 'y' _) = do
  copied <- focusDo $ rangeDo copier
  mapM_ (dispatchEvent . Copied) copied
  where
    copier :: Range -> BufAction String
    copier r = do
      str <- use (range r . asString)
      bufExt .= CopyPasta str
      return str
copyPasta (Keypress 'p' _) = focusDo paster
  where
    paster :: BufAction ()
    paster = do
      CopyPasta str <- use bufExt
      insertText (T.pack str)
copyPasta _ = return ()

copyListener :: Copied -> Action ()
copyListener (Copied str) = liftIO $ appendFile "copied.txt" ("Copied: " ++ str ++ "\n")

main :: IO ()
main = rasa $ do
  vim
  statusBar
  files
  cursors
  logger
  slate
  style
  eventListener copyPasta
  eventListener copyListener
