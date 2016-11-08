{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module State (
    St
  , text
  , render
) where

import Data.Monoid
import Control.Lens
import Data.Default (def, Default(..))

import qualified Data.Text as T

data St = St {
    _text :: T.Text
} deriving (Show)

instance Default St where
    def = St "Start"

makeLenses ''St

render :: St -> T.Text
render = view $ text.to (<> "_")
