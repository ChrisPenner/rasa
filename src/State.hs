{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module State (
    St
  , text
  , vHeight
  , render
) where

import Data.Monoid
import Data.List.Extra (takeEnd)
import Control.Monad.Reader
import Control.Arrow ((>>>))
import Control.Lens
import Data.Default (def, Default(..))

import qualified Data.Text as T

data St = St {
    _text :: T.Text
  , _vHeight :: Int
} deriving (Show)

instance Default St where
    def = St {
            _text="Start"
          , _vHeight=10
             }

makeLenses ''St

render :: St -> T.Text
render = applyViewport >>>
         addCursor >>>
         view text


addCursor :: St -> St
addCursor = over text (<> "_")

applyViewport :: St -> St
applyViewport = runReader . reader $ do
    viewportSize <- view vHeight
    ls <- T.lines . view text
    let window = T.unlines . getWindow viewportSize $ ls
    set text window
        where getWindow = takeEnd
