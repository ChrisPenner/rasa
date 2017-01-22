module ArbitraryInstances where

import qualified Data.Text as T
import qualified Yi.Rope as Y
import Test.QuickCheck
import Test.QuickCheck.Instances

instance Arbitrary Y.YiString where
  arbitrary = do
    txt <- arbitrary :: Gen T.Text
    return $ Y.fromText txt
