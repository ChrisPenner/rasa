module Rasa.Ext.VimSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Rasa.Ext.Vim" $ do
    context "Normal Mode" $ do
      it "moveRangesByC" $
        bufferTextTest "" "" $ do
