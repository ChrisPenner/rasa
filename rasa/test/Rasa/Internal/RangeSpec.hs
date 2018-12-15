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

import Data.List
import Data.Tree

import Control.Lens

import Data.IntervalMap.Generic.Interval (Interval(..), lowerBound, upperBound, rightClosed, overlaps, isEmpty)
import Data.IntervalMap.Generic.Strict (IntervalMap)
import qualified Data.IntervalMap as IM

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

    it "should deal with empty spans" $ do
      combineSpans "     " (
         [ ] :: [Span CrdRange String]
        )
      `shouldBe`
        [ (0, "") ]

    it "should deal with the unit spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 0)) "a")
        ]
      `shouldBe`
        [ (0, "a")
        , (1, "") ]

    -- A = aaa
    -- B =  b
    --     ---
    --     aaa.
    --      b

    it "should combine spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 2)) "a")
        , (Span (Range (Coord 0 1) (Coord 0 1)) "b")
        ]
      `shouldBe`
        [ (0,"a")
        , (1,"ab")
        , (2,"a")
        , (3,"")
        ]

    -- A = a
    -- B =  b
    --     --
    --     ab.

    it "should combine consecutive spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 0)) "a")
        , (Span (Range (Coord 0 1) (Coord 0 1)) "b")
        ]
      `shouldBe`
        [ (0,"a")
        , (1,"b")
        , (2,"")
        ]

    -- A = aaaa
    -- B =   b
    --     ----
    --     a aa.
    --       b

    it "should combine long spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 3)) "a")
        , (Span (Range (Coord 0 2) (Coord 0 2)) "b")
        ]
      `shouldBe`
        [ (0,"a")
        , (2,"ab")
        , (3,"a")
        , (4,"")
        ]

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

    it "should ignore spans with 0 size ranges" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 1) (Coord 0 0)) "a")
        , (Span (Range (Coord 0 1) (Coord 0 0)) "b")
        ]
      `shouldBe`
        [ (0,"")
        ]

    it "should combine spans containing mempty" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 1)) "")
        , (Span (Range (Coord 0 0) (Coord 0 1)) "")
        ]
      `shouldBe`
        [ (0, ""),
          (2, "")
        ]

    it "should combine a single span" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 1)) "a")
        ]
      `shouldBe`
        [ (0,"a")
        , (2,"")
        ]

    it "should combine full spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 1)) "a")
        , (Span (Range (Coord 0 0) (Coord 0 1)) "b")
        ]
      `shouldBe`
        [ (0,"ab")
        , (2,"")
        ]

    it "should combine three full spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 1)) "a")
        , (Span (Range (Coord 0 0) (Coord 0 1)) "b")
        , (Span (Range (Coord 0 0) (Coord 0 1)) "c")
        ]
      `shouldBe`
        [ (0,"abc")
        , (2,"")
        ]

    it "should ignore empty spans" $ do
      combineSpans "     "
        [ (Span (Range (Coord 0 0) (Coord 0 1)) "a")
        , (Span (Range (Coord 0 1) (Coord 0 0)) "b")
        ]
      `shouldBe`
        [ (0,"a")
        , (2,"")
        ]

    it "should combine multiline spans" $ do
      combineSpans "     \n     "
        [ (Span (Range (Coord 0 0) (Coord 1 2)) "a")
        , (Span (Range (Coord 1 1) (Coord 1 1)) "b")
        ]
      `shouldBe`
        [ (0,"a")
         ,(7,"ab")
         ,(8,"a")
         ,(9,"")
        ]
