module Rasa.Testing (
  testBufAction
  ) where

import Rasa.Internal.Interpreters
import Rasa.Internal.Actions
import Rasa.Internal.BufAction

import Test.Hspec
import Control.Monad
import qualified Yi.Rope as Y

testBufAction :: (Show a, Eq a) => String -> Y.YiString -> a -> BufAction a -> Spec
testBufAction description txt expectation bufAction = join . runIO $
  bootstrapAction $ do
    ref <- newBuffer txt
    mResult <- bufDo ref bufAction
    case mResult of
      Nothing -> error "(Err #75b72c) Buffer not found by BufRef"
      Just res -> return $ it description (res `shouldBe` expectation)
