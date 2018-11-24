module Rasa.Internal.RangeSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances                ( )
import           ArbitraryInstances                       ( )

import           Rasa.Internal.Range

mkSpan :: (Int, Int) -> (Int, Int) -> a -> Span CrdRange a
mkSpan (row1, col1) (row2, col2) val =
  Span (Range (Coord row1 col1) (Coord row2 col2)) val

spec :: Spec
spec = do
  describe "overRow" $ prop "overRow f ~= Coord (f a) b" $ \crd@(Coord r c) ->
    Coord (r + 1) c `shouldBe` overRow (+ 1) crd

  describe "overCol" $ prop "overCol f ~= Coord a (f b)" $ \crd@(Coord r c) ->
    Coord r (c + 1) `shouldBe` overCol (+ 1) crd

  describe "overBoth"
    $ prop "overBoth f ~= Coord (f a) (f b)"
    $ \crd@(Coord r c) ->
        Coord (r + 1) (c + 1) `shouldBe` overBoth (+ (1 :: Int)) crd

  describe "combineSpans" $ do
    it "should simply list non-overlapping spans"
      $ let rngs =
              [ mkSpan (0, 0) (0, 0) "a"
              , mkSpan (0, 1) (0, 1) "b"
              , mkSpan (0, 2) (0, 2) "c"
              ]
            expected =
              [ (Coord 0 0, "a")
              , (Coord 0 0, "")
              , (Coord 0 1, "b")
              , (Coord 0 1, "")
              , (Coord 0 2, "c")
              , (Coord 0 2, "")
              ]
        in  combineSpans rngs `shouldBe` expected

    it "should combine overlapping spans"
      $ let rngs = [mkSpan (0, 0) (0, 2) "a", mkSpan (0, 1) (0, 1) "b"]
            expected =
              [ (Coord 0 0, "a")
              , (Coord 0 1, "ab")
              , (Coord 0 2, "a")
              , (Coord 0 2, "")
              ]
        in  combineSpans rngs `shouldBe` expected
    it "should insert mempty in empty spans"
      $ let rngs = [mkSpan (0, 0) (0, 0) "a", mkSpan (0, 2) (0, 2) "b"]
            expected =
              [ (Coord 0 0, "a")
              , (Coord 0 1, "")
              , (Coord 0 2, "b")
              , (Coord 0 3, "")
              ]
        in  combineSpans rngs `shouldBe` expected
-- [((Coord (row 0) (col 0)),"a"),((Coord (row 0) (col 1)),"ab"),((Coord (row 0) (col 1)),"a"),((Coord (row 0) (col 2)),"")]
