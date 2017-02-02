module Rasa.Internal.BufActionsSpec where

import Test.Hspec

import Rasa.Testing
import Rasa.Internal.Range
import Rasa.Internal.BufActions
import qualified Yi.Rope as Y

sampleText :: Y.YiString
sampleText = "Testing line one\nshort line\n  a  long  line   "

-- spec :: Spec
-- spec =
--   describe "getLineRange" $
--     testBufAction "should get the range of a line" sampleText
--       (Just $ Range (Coord 1 0) (Coord 1 11)) (getLineRange 1)
