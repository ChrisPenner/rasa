{-# LANGUAGE
  Rank2Types
  , OverloadedStrings
  , DeriveFunctor
  , ScopedTypeVariables
  , TemplateHaskell
  , FlexibleInstances
  , MultiParamTypeClasses
  , ConstraintKinds
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
#-}
module Rasa.Internal.RangeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()
import ArbitraryInstances ()

import Rasa.Internal.Range

import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, Text())
import Data.Text.IO as TIO

import Data.Default
import Data.List
import Data.Tree

import Control.Lens

import Data.IntervalMap.Generic.Interval (Interval(..), lowerBound, upperBound, rightClosed, overlaps, isEmpty)
import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalMap as IM

newtype TestMonoid = TM String deriving (Show, Eq, Semigroup)

instance Monoid TestMonoid where
  mempty = TM ""

instance Default TestMonoid where
  def = TM "."

spec :: Spec
spec = do
  -- describe "overRow" $
  --   prop "overRow f ~= Coord (f a) b" $ \crd@(Coord r c) -> Coord (r+1) c `shouldBe` overRow (+1) crd
  --
  -- describe "overCol" $
  --   prop "overCol f ~= Coord a (f b)" $ \crd@(Coord r c) -> Coord r (c+1) `shouldBe` overCol (+1) crd
  --
  -- describe "overBoth" $
  --   prop "overBoth f ~= Coord (f a) (f b)" $ \crd@(Coord r c) -> Coord (r+1) (c+1) `shouldBe` overBoth (+ (1 :: Int)) crd

  describe "combineSpans" $ do
    -- it "noop" $ do
    --   noop `shouldReturn` ()

    it "should deal with empty spans"
      $ let rngs = [ ] :: [Span CrdRange TestMonoid]
            expected = [ ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should deal with the unit spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 0)) (TM "a"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a") ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should deal with the neutral element"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 0)) (TM "a"))
              , (Span (Range (Coord 0 0) (Coord 0 0)) (TM ""))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a") ]
        in  combineSpans "     " rngs `shouldBe` expected

    -- This would require to use a different appending operation
    -- than <> which would properly absorb def.
    -- See http://hackage.haskell.org/package/zero-0.1.4/docs/Data-Zero.html ?

    --
    -- it "should deal with the absorbing element"
    --   $ let rngs =
    --           [ (Span (Range (Coord 0 0) (Coord 0 0)) (TM "a"))
    --           , (Span (Range (Coord 0 0) (Coord 0 0)) (TM "."))
    --           ] :: [Span CrdRange TestMonoid]
    --         expected =
    --           [ (0, TM ".")
    --           , (1, TM ".") ]
    --     in  combineSpans "     " rngs `shouldBe` expected

    -- A =    aaa
    -- B =  b  b
    --     ---
    --      b aaa.
    --         b
    --
    -- it "should deal with defaults and transparency"
    --   $ let rngs =
    --           [ (Span (Range (Coord 0 0) (Coord 0 0)) (TM "a"))
    --           , (Span (Range (Coord 0 0) (Coord 0 0)) (TM ""))
    --           , (Span (Range (Coord 0 0) (Coord 0 0)) (TM "b"))
    --           ] :: [Span CrdRange TestMonoid]
    --         expected =
    --           [ (0, TM "a")
    --           , (1, TM ".") ]
    --     in  combineSpans "     " rngs `shouldBe` expected

    -- A = aaa
    -- B =  b
    --     ---
    --     aaa.
    --      b

    it "should combine spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 2)) (TM "a"))
              , (Span (Range (Coord 0 1) (Coord 0 1)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
              , (1, TM "ab")
              , (2, TM "a")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    -- A = a
    -- B =  b
    --     --
    --     ab.

    it "should combine consecutive spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 0)) (TM "a"))
              , (Span (Range (Coord 0 1) (Coord 0 1)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
              , (1, TM "b")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    -- A = aaaa
    -- B =   b
    --     ----
    --     a aa.
    --       b

    it "should combine long spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 3)) (TM "a"))
              , (Span (Range (Coord 0 2) (Coord 0 2)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
              , (2, TM "ab")
              , (3, TM "a")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    -- A = a.aa
    -- B =   b
    --     ----
    --     a.aa.
    --       b

    -- A = a..a
    -- B =   b
    --     ----
    --     a.ba.

    -- A = aa
    -- B =  bbb
    --     ----
    --     aab .
    --      b

    -- A = aaa
    -- B =  bbb
    -- B =   c
    --     ----
    --     aabb.
    --      bc

    it "should ignore spans with 0 size ranges"
      $ let rngs =
              [ (Span (Range (Coord 0 1) (Coord 0 0)) (TM "a"))
              , (Span (Range (Coord 0 1) (Coord 0 0)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should combine spans containing mempty"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 1)) (TM ""))
              , (Span (Range (Coord 0 0) (Coord 0 1)) (TM ""))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should combine a single span"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 1)) (TM "a"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should combine full spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 1)) (TM "a"))
              , (Span (Range (Coord 0 0) (Coord 0 1)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "ab")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should combine three full spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 1)) (TM "a"))
              , (Span (Range (Coord 0 0) (Coord 0 1)) (TM "b"))
              , (Span (Range (Coord 0 0) (Coord 0 1)) (TM "c"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "abc")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should ignore empty spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 0 1)) (TM "a"))
              , (Span (Range (Coord 0 1) (Coord 0 0)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
              ]
        in  combineSpans "     " rngs `shouldBe` expected

    it "should combine multiline spans"
      $ let rngs =
              [ (Span (Range (Coord 0 0) (Coord 1 2)) (TM "a"))
              , (Span (Range (Coord 1 1) (Coord 1 1)) (TM "b"))
              ] :: [Span CrdRange TestMonoid]
            expected =
              [ (0, TM "a")
               ,(7, TM "ab")
               ,(8, TM "a")
              ]
        in  combineSpans "     \n     " rngs `shouldBe` expected
