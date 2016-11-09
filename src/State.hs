{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module State (
    St
  , text
  , vHeight
  , render
) where

import Data.Monoid
import Data.List.Extra (takeEnd)
import Data.List (unfoldr)
import Control.Monad.Reader
import Control.Arrow ((>>>), second)
import Control.Lens
import Data.Default (def, Default(..))

import qualified Data.Text as T

data St = St {
    _text :: T.Text
  , _vHeight :: Int
} deriving (Show)

instance Default St where
    def = St {
            _text=""
          , _vHeight=10
             }

makeLenses ''St

render :: St -> T.Text
render = applyViewport
     >>> over text (textWrap 80)
     >>> addCursor
     >>> view text


addCursor :: St -> St
addCursor = over text (`T.snoc` '_')

applyViewport :: St -> St
applyViewport = runReader . reader $ do
    viewportSize <- view vHeight
    ls <- T.lines . view text
    let window = T.unlines . getWindow viewportSize $ ls
    set text window
        where getWindow = takeEnd


textWrap :: Int -> T.Text -> T.Text
textWrap n = T.dropEnd 1 . mconcat . unfoldr (splitLine n)

splitLine :: Int -> (T.Text -> Maybe (T.Text, T.Text))
splitLine n t
  | T.null t = Nothing
  | T.head t == '\n' = Just $ T.span (== '\n') t
  | T.compareLength (fst . splitAtNewline $ t) n == LT = Just $ splitAtNewline t
  | otherwise = Just $ second (T.append "\n-> ") $ T.splitAt n t

splitAtNewline :: T.Text -> (T.Text, T.Text)
splitAtNewline = T.span (/= '\n')
