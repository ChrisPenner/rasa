{-# LANGUAGE OverloadedStrings #-}
module Events (
                Event(..)
              , Mod(..)
    ) where


data Mod =
    Ctrl
  | Alt
  | Shift
  deriving (Show)

data Event =
    Keypress Char [Mod]
  | Esc
  | BS
  | Enter
  deriving (Show)


