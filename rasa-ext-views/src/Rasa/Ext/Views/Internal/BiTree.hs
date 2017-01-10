{-# language DeriveFunctor #-}
module Rasa.Ext.Views.Internal.BiTree where

data BiTreeF b l r =
  Branch b r r
    | Leaf l
    deriving (Show, Functor)




